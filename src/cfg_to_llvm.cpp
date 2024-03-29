#include "cfg_to_llvm.hpp"

#include "control_flow/node.hpp"
#include "llvm_type_lowering.hpp"
#include "operations.hpp"

#include <cassert>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h> // verifyModule

namespace {

    const std::map<std::string, bool> valid_bools{
        {"true",  true },
        {"True",  true },
        {"TRUE",  true },
        {"false", false},
        {"False", false},
        {"FALSE", false}
    };

    namespace constraints {

        constexpr size_t count_in(const char * text, char to_count) {
            auto count = 0U;
            while (text != nullptr and *text != '\0') {
                count += static_cast<unsigned int>(*text == to_count);
                // NOLINTNEXTLINE (*-pointer-arithmetic)
                ++text;
            }
            return count;
        }

        constexpr auto * first_part = "={ax},{ax},{di},{si},{dx},{r10},{r8},{r9},";
        constexpr auto count = count_in(first_part, ',') - 1;
        constexpr auto * suffix = ",~{r11},~{rcx},~{dirflag},~{fpsr},~{flags}";

    } // namespace constraints

    [[nodiscard]] std::array<std::string, constraints::count> generate_syscall_constraints_array() {
        using namespace constraints;
        std::array<std::string, count> result;

        auto index = 0U;
        // NOLINTNEXTLINE (*-pointer-arithmetic)
        for (const auto * iter = strchr(first_part, ',') + 1; iter != nullptr and *iter != '\0';
             // NOLINTNEXTLINE (*-pointer-arithmetic)
             ++iter) {

            if (*iter != ',') { continue; }

            auto constraints = std::string{first_part, static_cast<size_t>(iter - first_part)};

            // NOLINTNEXTLINE (*-pointer-arithmetic)
            result[index++] = constraints + suffix;
        }

        return result;
    }
} // namespace

cfg_to_llvm::~cfg_to_llvm() noexcept = default;

cfg_to_llvm::cfg_to_llvm(const std::string & name, llvm::LLVMContext & context,
                         global_map<std::string, llvm::GlobalObject *> & globals,
                         class llvm_type_lowering & typ_context)
    : context{context}
    , ir_module{std::make_unique<llvm::Module>(name, context)}
    , ir_builder{std::make_unique<llvm::IRBuilder<>>(context)}
    , globals{globals}
    , type_lowering{typ_context} {
    intrinsics.emplace("syscall", &cfg_to_llvm::syscall);
    intrinsics.emplace("arg_count", &cfg_to_llvm::arg_count);
    intrinsics.emplace("arg_at", &cfg_to_llvm::arg_at);

    ir_module->setTargetTriple(init_llvm_targets());
    ir_module->setSourceFileName(name);
}

cfg_to_llvm::node_data::node_data(llvm::IRBuilderBase & builder, llvm::Value * val,
                                  ast::type_ptr type)
    : value{val}
    , parent_block{builder.GetInsertBlock()}
    , ast_type{type} {}

void cfg_to_llvm::verify_module() const { llvm::verifyModule(*ir_module, &llvm::errs()); }

void cfg_to_llvm::dump() const { llvm::outs() << *ir_module << '\n'; }

void cfg_to_llvm::bind_value(const control_flow::node & node, llvm::Value * value,
                             ast::type_ptr type) {
    auto iter = values.find(&node);
    if (iter != values.end()) {
        llvm::outs() << "Previously value was " << *iter->second.value << "\nNow it is " << *value
                     << '\n';
    }
    assert(iter == values.end() or (iter->second.value == value and iter->second.ast_type == type));

    values.emplace(&node, node_data{*ir_builder, value, type});
}

const cfg_to_llvm::node_data * cfg_to_llvm::find_value_of(const control_flow::node * node) const {
    auto iter = values.find(node);
    return (iter != values.end()) ? &iter->second : nullptr;
}

template<typename OurOp, typename LLVMOp>
struct float_int_op_pair {
    OurOp our_op;
    LLVMOp int_op;
    LLVMOp float_op;
};

template<typename Result, size_t size, typename Pred>
[[nodiscard]] static constexpr const Result * find_in(const std::array<Result, size> & arr,
                                                      Pred predicate) {
    const auto * const iter = std::find_if(arr.begin(), arr.end(), predicate);
    return (iter != arr.end()) ? iter : nullptr;
}

void cfg_to_llvm::visit(control_flow::member_access & member_access) {
    const auto * lhs_value = find_value_of(member_access.lhs);
    assert(lhs_value != nullptr);

    assert(member_access.member_index.has_value());

    auto * llvm_struct_type = type_lowering.lower_to_llvm(lhs_value->ast_type);
    assert(llvm_struct_type != nullptr);

    auto * value = lhs_value->value;
    assert(value != nullptr);
    if (not value->getType()->isPointerTy()) {
        auto * slot = ir_builder->CreateAlloca(llvm_struct_type);
        ir_builder->CreateStore(value, slot);
        value = slot;
    }
    assert(value->getType()->isPointerTy());

    auto * pointer_to_result
        = ir_builder->CreateStructGEP(llvm_struct_type, value, *member_access.member_index);

    auto * result = ir_builder->CreateLoad(type_lowering.lower_to_llvm(member_access.result_type),
                                           pointer_to_result);

    bind_value(member_access, result, member_access.result_type);
    visited.emplace(&member_access);
    member_access.next->accept(*this);
}

static llvm::Value * lower_arithmetic_op(llvm::IRBuilderBase & ir_builder,
                                         llvm_type_lowering & type_lowering,
                                         llvm::Value * lhs_value, llvm::Value * rhs_value,
                                         operation::binary ast_bin_op, ast::type_ptr result_type) {

    if (lhs_value->getType()->isPointerTy() or rhs_value->getType()->isPointerTy()) {
        assert(lhs_value->getType()->isPointerTy() != rhs_value->getType()->isPointerTy());
        auto * pointer = lhs_value->getType()->isPointerTy() ? lhs_value : rhs_value;
        auto * index = lhs_value->getType()->isPointerTy() ? rhs_value : lhs_value;

        auto * pointer_type = llvm::dyn_cast_or_null<llvm::PointerType>(pointer->getType());
        assert(pointer_type != nullptr);
        auto * element_ty = type_lowering.lower_to_llvm(result_type);
        return ir_builder.CreateGEP(element_ty, pointer, index);
    }

    const auto is_constant
        = llvm::isa<llvm::Constant>(lhs_value) and llvm::isa<llvm::Constant>(rhs_value);
    const auto is_float
        = lhs_value->getType()->isFloatingPointTy() or rhs_value->getType()->isFloatingPointTy();

    using operand = operation::binary;
    using bin_ops = llvm::Instruction::BinaryOps;

    static constexpr std::array<float_int_op_pair<operand, bin_ops>, 4> arithmetic_ops{
        {
         {operand::add, bin_ops::Add, bin_ops::FAdd},
         {operand::sub, bin_ops::Sub, bin_ops::FSub},
         {operand::mult, bin_ops::Mul, bin_ops::FMul},
         {operand::div, bin_ops::SDiv, bin_ops::FDiv},
         }
    };

    const auto * selected_op = find_in(
        arithmetic_ops, [&ast_bin_op](auto & entry) -> bool { return entry.our_op == ast_bin_op; });

    assert(selected_op != nullptr);
    auto bin_op = is_float ? selected_op->float_op : selected_op->int_op;
    if (is_constant) {
        auto * constant_lhs = llvm::dyn_cast<llvm::Constant>(lhs_value);
        auto * constant_rhs = llvm::dyn_cast<llvm::Constant>(rhs_value);
        return llvm::ConstantExpr::get(bin_op, constant_lhs, constant_rhs);
    }

    return ir_builder.CreateBinOp(bin_op, lhs_value, rhs_value);
}

void cfg_to_llvm::visit(control_flow::binary_operation & binary_operation) {
    const auto * lhs_value = find_value_of(binary_operation.lhs);
    assert(lhs_value != nullptr);
    const auto * rhs_value = find_value_of(binary_operation.rhs);
    assert(rhs_value != nullptr);

    using predicate = llvm::CmpInst::Predicate;
    using operand = operation::binary;

    if (operation::is_shortcircuiting(binary_operation.op)
        or binary_operation.op == operation::binary::member_access) {
        assert(false);
    } else if (operation::is_comparison(binary_operation.op)) {
        static constexpr std::array<float_int_op_pair<operand, predicate>, 6> comparison_ops{
            {
             {operand::le, predicate::ICMP_SLE, predicate::FCMP_OLE},
             {operand::lt, predicate::ICMP_SLT, predicate::FCMP_OLT},
             {operand::ge, predicate::ICMP_SGE, predicate::FCMP_OGE},
             {operand::gt, predicate::ICMP_SGT, predicate::FCMP_OGT},
             {operand::eq, predicate::ICMP_EQ, predicate::FCMP_OEQ},
             {operand::ne, predicate::ICMP_NE, predicate::FCMP_ONE},
             }
        };

        const auto * selected_op
            = find_in(comparison_ops, [op = binary_operation.op](auto & entry) -> bool {
                  return entry.our_op == op;
              });

        assert(selected_op != nullptr);

        const auto is_constant = llvm::isa<llvm::Constant>(lhs_value->value)
                             and llvm::isa<llvm::Constant>(rhs_value->value);
        const auto is_float = lhs_value->value->getType()->isFloatingPointTy()
                           or rhs_value->value->getType()->isFloatingPointTy();

        if (is_constant) {
            auto pred = is_float ? selected_op->float_op : selected_op->int_op;
            auto * constant_lhs = llvm::dyn_cast<llvm::Constant>(lhs_value->value);
            auto * constant_rhs = llvm::dyn_cast<llvm::Constant>(rhs_value->value);
            bind_value(binary_operation,
                       llvm::ConstantExpr::getCompare(pred, constant_lhs, constant_rhs),
                       binary_operation.result_type);
        } else if (is_float) {
            bind_value(
                binary_operation,
                ir_builder->CreateFCmp(selected_op->float_op, lhs_value->value, rhs_value->value),
                binary_operation.result_type);
        } else {
            bind_value(
                binary_operation,
                ir_builder->CreateICmp(selected_op->int_op, lhs_value->value, rhs_value->value),
                binary_operation.result_type);
        }
    } else if (operation::is_arithmetic(binary_operation.op)) {
        bind_value(binary_operation,
                   lower_arithmetic_op(*ir_builder, type_lowering, lhs_value->value,
                                       rhs_value->value, binary_operation.op,
                                       binary_operation.result_type),
                   binary_operation.result_type);
    } else {
        assert(false);
    }

    visited.emplace(&binary_operation);
    binary_operation.next->accept(*this);
}

void cfg_to_llvm::visit(control_flow::branch & branch) {
    const auto * condition_value = find_value_of(branch.condition_value);
    assert(condition_value != nullptr);

    auto * start_block = ir_builder->GetInsertBlock();
    auto * current_function = start_block->getParent();

    // Generate the true branch
    auto * then_block = llvm::BasicBlock::Create(context, "", current_function);
    ir_builder->SetInsertPoint(then_block);
    branch.true_case->accept(*this);

    // The visit should only be marked after the true branch is finished.
    // This is due to the case of `if x then y();` without an else.
    // Our coresponding phi node is the false branch.
    visited.emplace(&branch);

    // Generate the else block
    auto * else_block = llvm::BasicBlock::Create(context, "", current_function);

    ir_builder->SetInsertPoint(start_block);
    ir_builder->CreateCondBr(condition_value->value, then_block, else_block);

    ir_builder->SetInsertPoint(else_block);
    branch.false_case->accept(*this);
}

void cfg_to_llvm::visit(control_flow::cast & cast) {
    const auto * src = find_value_of(cast.value);
    auto * llvm_dest_type = type_lowering.lower_to_llvm(cast.type);

    assert(src->value->getType()->isIntegerTy() and llvm_dest_type->isIntegerTy());

    // TODO: Determine what other kind of cast to use
    bind_value(cast,
               ir_builder->CreateCast(llvm::Instruction::CastOps::SExt, src->value, llvm_dest_type),
               cast.type);

    visited.emplace(&cast);
    cast.next->accept(*this);
}

void cfg_to_llvm::visit(control_flow::constant & constant) {

    switch (constant.val_type) {
    case literal_type::identifier:
        assert(std::holds_alternative<std::string>(constant.value));
        for (auto & scope : local_names) {
            if (auto iter = scope.find(std::get<std::string>(constant.value));
                iter != scope.end()) {
                assert(constant.type != nullptr);
                bind_value(constant, iter->second, constant.type);
                break;
            }
        }
        break;
    case literal_type::null: {
        auto * llvm_ptr_type = llvm::dyn_cast_if_present<llvm::PointerType>(
            type_lowering.lower_to_llvm(constant.type));
        assert(llvm_ptr_type != nullptr);
        bind_value(constant, llvm::ConstantPointerNull::get(llvm_ptr_type), constant.type);
    } break;
    case literal_type::integer: {
        auto * llvm_int_type = llvm::dyn_cast_if_present<llvm::IntegerType>(
            type_lowering.lower_to_llvm(constant.type));
        assert(llvm_int_type != nullptr);
        assert(std::holds_alternative<long>(constant.value));
        bind_value(constant, llvm::ConstantInt::get(llvm_int_type, std::get<long>(constant.value)),
                   constant.type);
    } break;
    case literal_type::floating:
        assert(false and "Implement floating point IR");
        break;
    case literal_type::character:
        assert(std::holds_alternative<char>(constant.value));
        bind_value(
            constant,
            llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), std::get<char>(constant.value)),
            constant.type);
        break;
    case literal_type::boolean:
        assert(std::holds_alternative<bool>(constant.value));
        bind_value(constant, llvm::ConstantInt::getBool(context, std::get<bool>(constant.value)),
                   constant.type);
        break;
    case literal_type::string:
        assert(std::holds_alternative<std::string>(constant.value));
        bind_value(constant,
                   ir_builder->CreateGlobalStringPtr(std::get<std::string>(constant.value)),
                   constant.type);
        break;
    }

    assert(values.find(&constant) != values.end());
    visited.emplace(&constant);
    constant.next->accept(*this);
}

void cfg_to_llvm::visit(control_flow::function_call & func_call) {
    const auto * node_data = find_value_of(func_call.callee);
    assert(node_data != nullptr);
    auto * func = llvm::dyn_cast_or_null<llvm::Function>(node_data->value);
    assert(func != nullptr);

    auto * func_type = func->getFunctionType();

    std::vector<llvm::Value *> args;
    args.reserve(func_call.arguments.size());

    for (auto & argument : func_call.arguments) {
        const auto * node_data = find_value_of(argument);
        assert(node_data != nullptr);

        auto * llvm_value = node_data->value;
        if (dynamic_cast<const ast::struct_type *>(node_data->ast_type) != nullptr) {
            llvm_value = ir_builder->CreateLoad(type_lowering.lower_to_llvm(node_data->ast_type),
                                                llvm_value);
        }

        args.push_back(llvm_value);
    }

    bind_value(func_call, ir_builder->CreateCall(func_type, func, args),
               func_call.callee->type->return_type());
    visited.emplace(&func_call);
    func_call.next->accept(*this);
}

void cfg_to_llvm::visit(control_flow::function_end & func_end) {
    visited.emplace(&func_end);

    if (func_end.value == nullptr) {
        ir_builder->CreateRetVoid();
        return;
    }

    const auto * node_data = find_value_of(func_end.value);
    assert(node_data != nullptr);
    auto * value = node_data->value;
    assert(value != nullptr);

    auto * ret_type = ir_builder->getCurrentFunctionReturnType();
    if (ret_type->getPointerTo() == value->getType()) {
        // "Auto load"
        value = ir_builder->CreateLoad(ret_type, value);
    }
    assert(ret_type == value->getType());

    ir_builder->CreateRet(value);
}

void cfg_to_llvm::visit(control_flow::function_start & func_start) {

    auto * func_type
        = llvm::cast_or_null<llvm::FunctionType>(type_lowering.lower_to_llvm(func_start.type));
    assert(func_type != nullptr);

    // The only functions that need ExternalLinkage are "main" or exported ones
    auto linkage = (func_start.name == "main" or func_start.exported)
                     ? llvm::Function::ExternalLinkage
                     : llvm::Function::InternalLinkage;

    auto * func = llvm::Function::Create(func_type, linkage, func_start.name, ir_module.get());

    // add the function to the current scope
    bind_value(func_start, func, func_start.type);

    auto * block = llvm::BasicBlock::Create(context, func->getName(), func);
    ir_builder->SetInsertPoint(block);

    auto & func_scope = local_names.add_scope();
    for (auto i = 0UL; i < func_start.parameter_names.size(); ++i) {
        llvm::Value * llvm_value = func->getArg(i);

        if (llvm_value->getType()->isStructTy()) {
            // We need to copy the struct to ourselves
            auto * slot = ir_builder->CreateAlloca(llvm_value->getType());
            ir_builder->CreateStore(llvm_value, slot);
            llvm_value = slot;
        }

        llvm_value->setName(func_start.parameter_names[i]);
        func_scope.emplace(func_start.parameter_names[i], llvm_value);
    }
    local_names.add_to_root(func_start.name, func);

    visited.emplace(&func_start);
    func_start.next->accept(*this);

    // Ensure termination for the whole function
    // TODO: Iterate every block and check for termination on each?
    if (auto & last_bb = func->back(); not last_bb.back().isTerminator()) {
        ir_builder->SetInsertPoint(&last_bb);
        if (auto * return_type = func_type->getReturnType(); return_type->isVoidTy()) {
            ir_builder->CreateRetVoid();
        } else if (func_start.name == "main" and return_type->isIntegerTy()) {
            // Like C++, we add a `return 0` at the end.
            ir_builder->CreateRet(llvm::ConstantInt::get(return_type, 0));
        } else {
            dump();
            assert(false);
        }
    }

    if (func_start.exported) {
        globals.add(ir_module->getModuleIdentifier(), func_start.name, func);
    }

    local_names.remove_scope();
}

void cfg_to_llvm::arg_at(control_flow::intrinsic_call & intrinsic_call) {

    assert(intrinsic_call.arguments.size() == 1);
    auto * arg_cfg = intrinsic_call.arguments.front();
    assert(arg_cfg != nullptr);

    const auto * arg_data = find_value_of(arg_cfg);
    assert(arg_data != nullptr);

    auto * arg_ir = arg_data->value;
    assert(arg_ir != nullptr);

    assert(intrinsic_call.type != nullptr);

    auto * func_type
        = llvm::cast_or_null<llvm::FunctionType>(type_lowering.lower_to_llvm(intrinsic_call.type));
    assert(func_type != nullptr);

    auto * arg_at_ir = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, "arg_at",
                                              ir_module.get());

    bind_value(intrinsic_call, ir_builder->CreateCall(func_type, arg_at_ir, {arg_ir}),
               intrinsic_call.type);
}

void cfg_to_llvm::arg_count(control_flow::intrinsic_call & intrinsic_call) {
    auto * func_type
        = llvm::cast_or_null<llvm::FunctionType>(type_lowering.lower_to_llvm(intrinsic_call.type));
    assert(func_type != nullptr);

    auto * arg_count_ir = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage,
                                                 "arg_count", ir_module.get());

    bind_value(intrinsic_call, ir_builder->CreateCall(func_type, arg_count_ir),
               intrinsic_call.type);
}

void cfg_to_llvm::syscall(control_flow::intrinsic_call & intrinsic_call) {

    std::vector<llvm::Value *> args;
    args.reserve(intrinsic_call.arguments.size());

    for (auto * arg : intrinsic_call.arguments) {
        const auto * arg_data = find_value_of(arg);
        assert(arg_data != nullptr);
        args.push_back(arg_data->value);
    }

    static const auto syscall_constraints = generate_syscall_constraints_array();

    const auto & constraint = syscall_constraints.at(intrinsic_call.arguments.size() - 1);

    assert(intrinsic_call.type != nullptr);

    auto * func_type
        = llvm::cast_or_null<llvm::FunctionType>(type_lowering.lower_to_llvm(intrinsic_call.type));
    assert(func_type != nullptr);

    // TODO: Submit bug to fix this
    if (auto verify_result = llvm::InlineAsm::verify(func_type, constraint);
        verify_result.success()) {
        llvm::errs() << verify_result << '\n';
    }

    bind_value(intrinsic_call,
               ir_builder->CreateCall(
                   func_type, llvm::InlineAsm::get(func_type, "syscall", constraint, true), args),
               intrinsic_call.type->return_type());
}

void cfg_to_llvm::visit(control_flow::intrinsic_call & intrinsic_call) {
    if (auto iter = intrinsics.find(intrinsic_call.name); iter != intrinsics.end()) {
        (this->*iter->second)(intrinsic_call);
    } else {
        assert(false);
    }

    visited.emplace(&intrinsic_call);
    intrinsic_call.next->accept(*this);
}

void cfg_to_llvm::patch_parent_block(llvm::Function * current_function,
                                     std::vector<cfg_to_llvm::node_data> & values,
                                     node_data & incoming_value, llvm::BasicBlock * phi_block) {

    for (auto & block : *current_function) {
        if (std::find_if(values.begin(), values.end(),
                         [&block](const node_data & value) { return value.parent_block == &block; })
            != values.end()) {
            continue;
        }

        auto * terminator = llvm::dyn_cast_if_present<llvm::BranchInst>(block.getTerminator());
        if (terminator == nullptr) { continue; }

        if (std::find_if(terminator->successors().begin(), terminator->successors().end(),
                         [&phi_block](auto * succ) { return succ == phi_block; })
            == terminator->successors().end()) {
            continue;
        }

        incoming_value.parent_block = &block;
        return;
    }

    assert(false);
}

void cfg_to_llvm::visit(control_flow::phi & phi) {
    bool should_continue = true;
    for (auto * prev : phi.previous) {
        if (visited.find(prev) == visited.end()) {
            should_continue = false;
            break;
        }
    }

    if (not should_continue) { return; }

    auto * start_block = ir_builder->GetInsertBlock();
    auto * current_function = start_block->getParent();
    auto * phi_block = start_block->empty()
                         ? start_block
                         : llvm::BasicBlock::Create(context, "", current_function);

    // HACK: Sometimes, phi nodes are values and need a type.
    //       This will happen if all previous nodes have the same type.

    std::vector<node_data> values;

    std::optional<llvm::Type *> phi_type;
    for (auto * prev : phi.previous) {
        const auto * prev_value = find_value_of(prev);
        // No value from one or more paths in => control flow only (i.e. if statement)
        if (prev_value == nullptr) {
            phi_type = nullptr;
            break;
        }

        values.emplace_back(*prev_value);

        assert(prev_value->value != nullptr);
        if (auto * prev_type = prev_value->value->getType(); not phi_type.has_value()) {
            phi_type = prev_type;
        } else if (phi_type != prev_type) {
            phi_type = nullptr;
        }

        // HACK: This is done for constants in if expressions,
        // since they need to come from the previous block,
        // but we may have added them to our current one.
        if (prev_value->parent_block != phi_block) {
            ir_builder->SetInsertPoint(prev_value->parent_block);
            ir_builder->CreateBr(phi_block);
        }
    }

    ir_builder->SetInsertPoint(phi_block);
    assert(phi_type.has_value());

    if (phi_type != nullptr) {
        // HACK
        auto * llvm_phi = ir_builder->CreatePHI(*phi_type, values.size());
        for (auto incoming_value : values) {
            // HACK: This is done for constants in if expressions,
            // since they need to come from the previous block,
            // but we may have added them to our current one.
            if (incoming_value.parent_block == phi_block) {
                patch_parent_block(current_function, values, incoming_value, phi_block);
            }
            llvm_phi->addIncoming(incoming_value.value, incoming_value.parent_block);
        }
        bind_value(phi, llvm_phi, phi.type);
    }

    visited.emplace(&phi);
    phi.next->accept(*this);
}

void cfg_to_llvm::visit(control_flow::struct_init & struct_init) {

    auto * llvm_struct_type = type_lowering.lower_to_llvm(struct_init.result_type);
    assert(llvm_struct_type != nullptr);

    auto * slot = ir_builder->CreateAlloca(llvm_struct_type);

    for (auto & [name, expr] : struct_init.fields) {

        auto index = struct_init.result_type->field_count();
        for (auto i = 0UL; i < struct_init.result_type->field_count(); ++i) {
            if (struct_init.result_type->field(i).first == name) {
                index = i;
                break;
            }
        }

        assert(index < struct_init.result_type->field_count());

        auto * pointer_to_dest = ir_builder->CreateStructGEP(llvm_struct_type, slot, index);

        ir_builder->CreateStore(find_value_of(expr)->value, pointer_to_dest);
    }

    bind_value(struct_init, slot, struct_init.result_type);

    visited.emplace(&struct_init);
    struct_init.next->accept(*this);
}

void cfg_to_llvm::visit(control_flow::unary_operation & unary_operation) {

    const auto * value = find_value_of(unary_operation.operand);
    assert(value != nullptr);
    auto * const_val = llvm::dyn_cast<llvm::Constant>(value->value);

    using operand = operation::unary;
    switch (unary_operation.op) {
    case operand::bool_not:
        if (const_val != nullptr) {
            bind_value(unary_operation, llvm::ConstantExpr::getNot(const_val),
                       unary_operation.result_type);
        } else {
            bind_value(unary_operation, ir_builder->CreateNot(value->value),
                       unary_operation.result_type);
        }
        break;
    case operand::deref: {
        assert(const_val == nullptr);

        auto * element_ty = type_lowering.lower_to_llvm(unary_operation.result_type);

        // TODO: Align it?
        bind_value(unary_operation, ir_builder->CreateLoad(element_ty, value->value),
                   unary_operation.result_type);
    } break;
    case operand::negate:
        if (auto float_op = value->value->getType()->isFloatingPointTy();
            const_val != nullptr and not float_op) {
            bind_value(unary_operation, llvm::ConstantExpr::getNeg(const_val),
                       unary_operation.result_type);
        } else {
            bind_value(unary_operation,
                       float_op ? ir_builder->CreateFNeg(value->value)
                                : ir_builder->CreateNeg(value->value),
                       unary_operation.result_type);
        }
        break;
    case operand::addrof:
        if (const_val != nullptr) {
            auto * slot = ir_builder->CreateAlloca(type_lowering.lower_to_llvm(value->ast_type));
            ir_builder->CreateStore(const_val, slot);
            bind_value(unary_operation, slot, unary_operation.result_type);
        } else if (auto * alloca_inst = llvm::dyn_cast<llvm::AllocaInst>(value->value);
                   alloca_inst != nullptr) {
            bind_value(unary_operation, alloca_inst, unary_operation.result_type);
        } else {
            assert(false);
        }
        break;
    }

    visited.emplace(&unary_operation);
    unary_operation.next->accept(*this);
}
