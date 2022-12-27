#include "cfg_to_llvm.hpp"

#include "control_flow/node.hpp"
#include "llvm_type_lowering.hpp"
#include "operations.hpp"

#include <cassert>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Verifier.h> // verifyModule

namespace {

    const std::map<std::string, bool> valid_bools{{"true", true},   {"True", true},
                                                  {"TRUE", true},   {"false", false},
                                                  {"False", false}, {"FALSE", false}};

    namespace constraints {

        // NOLINTNEXTLINE
        constexpr size_t count_in(const char * text, char c) {
            auto count = 0U;
            while (text != nullptr and *text != '\0') {
                count += static_cast<unsigned int>(*text == c);
                // NOLINTNEXTLINE (*-pointer-arithmetic)
                ++text;
            }
            return count;
        }

        constexpr auto * first_part = "=A,A,{di},{si},{dx},{r10},{r8},{r9},";
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
    , type_context{typ_context}
    , intrinsics{{"syscall", &cfg_to_llvm::syscall}} {
    ir_module->setTargetTriple(init_llvm_targets());
}

cfg_to_llvm::node_data::node_data(llvm::IRBuilderBase & builder, llvm::Value * val)
    : value{val}
    , parent_block{builder.GetInsertBlock()} {}

void cfg_to_llvm::verify_module() const { llvm::verifyModule(*ir_module, &llvm::errs()); }

void cfg_to_llvm::dump() const { llvm::outs() << *ir_module << '\n'; }

void cfg_to_llvm::bind_value(const control_flow::node & node, llvm::Value * value) {
    assert(values.find(&node) == values.end());

    values.emplace(&node, node_data{*ir_builder, value});
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
[[nodiscard]] static constexpr std::optional<Result> find_in(const std::array<Result, size> & arr,
                                                             Pred predicate) {
    const auto iter = std::find_if(arr.begin(), arr.end(), predicate);
    return (iter != arr.end()) ? std::optional{*iter} : std::nullopt;
}

// TODO: Use lookup table(s)
void cfg_to_llvm::visit(control_flow::binary_operation & binary_operation) {
    const auto * lhs_value = find_value_of(binary_operation.lhs);
    assert(lhs_value != nullptr);
    const auto * rhs_value = find_value_of(binary_operation.rhs);
    assert(rhs_value != nullptr);

    const auto is_constant = llvm::isa<llvm::Constant>(lhs_value->value)
                         and llvm::isa<llvm::Constant>(rhs_value->value);
    const auto is_float = lhs_value->value->getType()->isFloatingPointTy()
                       or rhs_value->value->getType()->isFloatingPointTy();

    using predicate = llvm::CmpInst::Predicate;
    using operand = operation::binary;

    if (operation::is_shortcircuiting(binary_operation.op)) {
        assert(false);
    } else if (operation::is_comparison(binary_operation.op)) {
        static constexpr std::array<float_int_op_pair<operand, predicate>, 6> comparison_ops{{
            {operand::le, predicate::ICMP_SGE, predicate::FCMP_OLE},
            {operand::lt, predicate::ICMP_SLT, predicate::FCMP_OLT},
            {operand::ge, predicate::ICMP_SGE, predicate::FCMP_OGE},
            {operand::gt, predicate::ICMP_SGT, predicate::FCMP_OGT},
            {operand::eq, predicate::ICMP_EQ, predicate::FCMP_OEQ},
            {operand::ne, predicate::ICMP_NE, predicate::FCMP_ONE},
        }};

        auto selected_op
            = find_in(comparison_ops, [op = binary_operation.op](auto & entry) -> bool {
                  return entry.our_op == op;
              });

        assert(selected_op.has_value());

        if (is_constant) {
            auto pred = is_float ? selected_op->float_op : selected_op->int_op;
            auto * constant_lhs = llvm::dyn_cast<llvm::Constant>(lhs_value->value);
            auto * constant_rhs = llvm::dyn_cast<llvm::Constant>(rhs_value->value);
            bind_value(binary_operation,
                       llvm::ConstantExpr::getCompare(pred, constant_lhs, constant_rhs));
        } else if (is_float) {
            bind_value(
                binary_operation,
                ir_builder->CreateFCmp(selected_op->float_op, lhs_value->value, rhs_value->value));
        } else {
            bind_value(
                binary_operation,
                ir_builder->CreateICmp(selected_op->int_op, lhs_value->value, rhs_value->value));
        }
    } else if (operation::is_arithmetic(binary_operation.op)) {

        llvm::Value * result = nullptr;

        if (lhs_value->value->getType()->isPointerTy()
            or rhs_value->value->getType()->isPointerTy()) {
            assert(lhs_value->value->getType()->isPointerTy()
                   != rhs_value->value->getType()->isPointerTy());
            auto * pointer
                = (lhs_value->value->getType()->isPointerTy() ? lhs_value : rhs_value)->value;
            auto * index
                = (lhs_value->value->getType()->isPointerTy() ? rhs_value : lhs_value)->value;

            auto * pointer_type = llvm::dyn_cast_or_null<llvm::PointerType>(pointer->getType());
            assert(pointer_type != nullptr);
            auto * element_ty = pointer_type->getPointerElementType();
            result = ir_builder->CreateGEP(element_ty, pointer, index);
        }

        using operand = operation::binary;
        using bin_ops = llvm::Instruction::BinaryOps;

        static constexpr std::array<float_int_op_pair<operand, bin_ops>, 4> arithmetic_ops{{
            {operand::add, bin_ops::Add, bin_ops::FAdd},
            {operand::sub, bin_ops::Sub, bin_ops::FSub},
            {operand::mult, bin_ops::Mul, bin_ops::FMul},
            {operand::div, bin_ops::SDiv, bin_ops::FDiv},
        }};

        auto selected_op
            = find_in(arithmetic_ops, [op = binary_operation.op](auto & entry) -> bool {
                  return entry.our_op == op;
              });

        if (result == nullptr) {
            assert(selected_op.has_value());
            auto bin_op = is_float ? selected_op->float_op : selected_op->int_op;
            if (is_constant) {
                auto * constant_lhs = llvm::dyn_cast<llvm::Constant>(lhs_value->value);
                auto * constant_rhs = llvm::dyn_cast<llvm::Constant>(rhs_value->value);
                result = llvm::ConstantExpr::get(bin_op, constant_lhs, constant_rhs);
            } else {
                result = ir_builder->CreateBinOp(bin_op, lhs_value->value, rhs_value->value);
            }
        }

        bind_value(binary_operation, result);
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

    visited.emplace(&branch);

    // Generate the true branch
    auto * then_block = llvm::BasicBlock::Create(context, "", current_function);
    ir_builder->SetInsertPoint(then_block);
    branch.true_case->accept(*this);

    // Generate the else block
    auto * else_block = llvm::BasicBlock::Create(context, "", current_function);
    ir_builder->SetInsertPoint(else_block);
    branch.false_case->accept(*this);

    ir_builder->SetInsertPoint(start_block);
    ir_builder->CreateCondBr(condition_value->value, then_block, else_block);
}

void cfg_to_llvm::visit(control_flow::constant & constant) {

    switch (constant.val_type) {
    case literal_type::identifier:
        assert(std::holds_alternative<std::string>(constant.value));
        for (auto & scope : local_names) {
            if (auto iter = scope.find(std::get<std::string>(constant.value));
                iter != scope.end()) {
                bind_value(constant, iter->second);
                break;
            }
        }
        break;
    case literal_type::null:
        assert(false and "Store the type of a pointer for null");
        break;
    case literal_type::integer:
        assert(std::holds_alternative<long>(constant.value));
        bind_value(constant, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                                    std::get<long>(constant.value)));
        break;
    case literal_type::floating:
        assert(false and "Implement floating point IR");
        break;
    case literal_type::character:
        assert(std::holds_alternative<char>(constant.value));
        bind_value(constant, llvm::ConstantInt::get(llvm::Type::getInt8Ty(context),
                                                    std::get<char>(constant.value)));
        break;
    case literal_type::boolean:
        assert(std::holds_alternative<bool>(constant.value));
        bind_value(constant, llvm::ConstantInt::getBool(context, std::get<bool>(constant.value)));
        break;
    case literal_type::string:
        assert(std::holds_alternative<std::string>(constant.value));
        bind_value(constant,
                   ir_builder->CreateGlobalStringPtr(std::get<std::string>(constant.value)));
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
        args.push_back(node_data->value);
    }

    bind_value(func_call, ir_builder->CreateCall(func_type, func, args));
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
    ir_builder->CreateRet(node_data->value);
}

void cfg_to_llvm::visit(control_flow::function_start & func_start) {

    std::vector<llvm::Type *> param_types;
    param_types.reserve(func_start.arg_count);

    for (auto i = 0U; i < func_start.arg_count; ++i) {
        auto ast_type = func_start.type->arg(i);
        auto * llvm_param_type = type_context.lower_to_llvm(ast_type);
        if (dynamic_cast<ast::struct_type *>(ast_type.get()) != nullptr) {
            // All structs need to be passed as pointers
            llvm_param_type = llvm_param_type->getPointerTo();
        }
        param_types.push_back(llvm_param_type);
    }

    auto * func_type = llvm::FunctionType::get(
        type_context.lower_to_llvm(func_start.type->return_type()), param_types, false);

    // The only functions that need ExternalLinkage are "main" or exported ones
    auto linkage = (func_start.name == "main" or func_start.exported)
                     ? llvm::Function::ExternalLinkage
                     : llvm::Function::InternalLinkage;

    auto * func = llvm::Function::Create(func_type, linkage, func_start.name, ir_module.get());

    // add the function to the current scope
    bind_value(func_start, func);

    auto & func_scope = local_names.add_scope();
    for (auto i = 0UL; i < func_start.parameter_names.size(); ++i) {
        auto * llvm_value = func->getArg(i);
        llvm_value->setName(func_start.parameter_names[i]);
        func_scope.emplace(func_start.parameter_names[i], llvm_value);
    }
    local_names.add_to_root(func_start.name, func);

    auto * block = llvm::BasicBlock::Create(context, func->getName(), func);
    ir_builder->SetInsertPoint(block);

    visited.emplace(&func_start);
    func_start.next->accept(*this);

    // Ensure termination for the whole function
    // TODO: Iterate every block and check for termination on each?
    if (auto & last_bb = func->getBasicBlockList().back(); not last_bb.back().isTerminator()) {
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

    std::vector<llvm::Type *> param_types;
    param_types.reserve(args.size());
    for (auto * val : args) { param_types.push_back(val->getType()); }

    auto * func_type = llvm::FunctionType::get(type_context.lower_to_llvm(ast::prim_type::int32),
                                               param_types, false);

    assert(llvm::InlineAsm::Verify(func_type, constraint));

    bind_value(intrinsic_call,
               ir_builder->CreateCall(
                   func_type, llvm::InlineAsm::get(func_type, "syscall", constraint, true), args));
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
    auto * phi_block = llvm::BasicBlock::Create(context, "", current_function);

    // HACK: Sometimes, phi nodes are values and need a type.
    //       This will happen if all previous nodes have the same type.

    std::vector<node_data> values;

    std::optional<llvm::Type *> phi_type;
    for (auto * prev : phi.previous) {
        const auto * prev_value = find_value_of(prev);
        assert(prev_value != nullptr);

        values.emplace_back(*prev_value);

        assert(prev_value->value != nullptr);
        if (auto * prev_type = prev_value->value->getType(); not phi_type.has_value()) {
            phi_type = prev_type;
        } else if (phi_type != prev_type) {
            phi_type = nullptr;
        }

        llvm::BranchInst::Create(phi_block, prev_value->parent_block);
    }

    ir_builder->SetInsertPoint(phi_block);
    assert(phi_type.has_value());

    if (phi_type != nullptr) {
        // HACK
        auto * llvm_phi = ir_builder->CreatePHI(*phi_type, values.size());
        for (auto incoming_value : values) {
            llvm_phi->addIncoming(incoming_value.value, incoming_value.parent_block);
        }
        bind_value(phi, llvm_phi);
    }

    visited.emplace(&phi);
    phi.next->accept(*this);
}

void cfg_to_llvm::visit(control_flow::unary_operation & unary_operation) {

    const auto * value = find_value_of(unary_operation.operand);
    assert(value != nullptr);
    auto * const_val = llvm::dyn_cast<llvm::Constant>(value->value);

    using operand = operation::unary;
    switch (unary_operation.op) {
    case operand::bool_not:
        if (const_val != nullptr) {
            bind_value(unary_operation, llvm::ConstantExpr::getNot(const_val));
        } else {
            bind_value(unary_operation, ir_builder->CreateNot(value->value));
        }
        break;
    case operand::deref: {
        assert(const_val == nullptr);

        auto * ptr_ty = llvm::dyn_cast<llvm::PointerType>(value->value->getType());
        assert(ptr_ty != nullptr);

        // TODO: Align it?
        bind_value(unary_operation,
                   ir_builder->CreateLoad(ptr_ty->getPointerElementType(), value->value));
    } break;
    case operand::negate:
        if (auto float_op = value->value->getType()->isFloatingPointTy(); const_val != nullptr) {
            bind_value(unary_operation, float_op ? llvm::ConstantExpr::getFNeg(const_val)
                                                 : llvm::ConstantExpr::getNeg(const_val));
        } else {
            bind_value(unary_operation, float_op ? ir_builder->CreateFNeg(value->value)
                                                 : ir_builder->CreateNeg(value->value));
        }
        break;
    }

    unary_operation.next->accept(*this);
}
