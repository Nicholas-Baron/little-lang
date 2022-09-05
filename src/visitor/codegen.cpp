#include "codegen.hpp"

#include "ast/nodes.hpp"
#include "ast/type.hpp"
#include "emit_asm.hpp"
#include "token_to_string.hpp"
#include "type_context.hpp"

#include <sstream>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>

namespace visitor {

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

        std::array<std::string, constraints::count> generate_syscall_constraints_array() {
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

    codegen::codegen(const std::string & name, llvm::LLVMContext & context,
                     global_map<std::string, llvm::GlobalObject *> & program_globals,
                     class type_context & typ_context)
        : context{context}
        , ir_module{std::make_unique<llvm::Module>(name, context)}
        , ir_builder{std::make_unique<llvm::IRBuilder<>>(context)}
        , type_context(typ_context)
        , program_globals{program_globals}
        , instrinics{{"syscall", &codegen::syscall},
                     {"arg_count", &codegen::arg_count},
                     {"arg_at", &codegen::arg_at}} {
        ir_module->setTargetTriple(init_llvm_targets());
    }

    void codegen::dump() const { llvm::outs() << *ir_module << '\n'; }

    llvm::Value * codegen::find_alive_value(const std::string & name, bool should_error) const {
        // Walk backwards thru scopes
        for (const auto & scope : active_values) {
            if (auto iter = scope.find(name); iter != scope.end()) {

                // Globals are always pointers to data, so we should try to use the initializer
                auto * global = llvm::dyn_cast<llvm::GlobalVariable>(iter->second);
                if (global != nullptr and global->hasInitializer()) {
                    return global->getInitializer();
                }
                return iter->second;
            }
        }
        if (should_error) { printError(std::nullopt, "Could not find value ", name); }
        return nullptr;
    }

    llvm::Type * codegen::find_type(const ast::type_ptr & type, std::optional<Location> loc) {

        auto * typ = type_context.lower_to_llvm(type);
        if (typ == nullptr) { printError(loc, type, " is not a valid type"); }
        return typ;
    }

    void codegen::evaluate_comparison(ast::binary_expr & binary_expr, llvm::Value * lhs_value,
                                      llvm::Value * rhs_value, bool is_float, bool is_constant) {

        assert(lhs_value != nullptr and rhs_value != nullptr);
        using operand = ast::binary_expr::operand;

        using predicate = llvm::CmpInst::Predicate;
        auto int_or_float = [&is_float](predicate int_pred, predicate float_pred) {
            return is_float ? float_pred : int_pred;
        };

        std::optional<predicate> p;
        switch (binary_expr.op) {
        case operand::le:
            p = int_or_float(predicate::ICMP_SLE, predicate::FCMP_OLE);
            break;
        case operand::lt:
            p = int_or_float(predicate::ICMP_SLT, predicate::FCMP_OLT);
            break;
        case operand::ge:
            p = int_or_float(predicate::ICMP_SGE, predicate::FCMP_OGE);
            break;
        case operand::gt:
            p = int_or_float(predicate::ICMP_SGT, predicate::FCMP_OGT);
            break;
        case operand::eq:
            p = int_or_float(predicate::ICMP_EQ, predicate::FCMP_OEQ);
            break;
        case operand::ne:
            p = int_or_float(predicate::ICMP_NE, predicate::FCMP_ONE);
            break;
        default:
            printError(binary_expr.location(), "Comparison operator ",
                       tok_to_string(binary_expr.op), " is not implemented yet");
        }
        assert(p.has_value());
        if (is_constant) {
            auto * constant_lhs = llvm::dyn_cast<llvm::Constant>(lhs_value);
            auto * constant_rhs = llvm::dyn_cast<llvm::Constant>(rhs_value);
            store_result(llvm::ConstantExpr::getCompare(*p, constant_lhs, constant_rhs));
        } else if (is_float) {
            store_result(ir_builder->CreateFCmp(*p, lhs_value, rhs_value));
        } else {
            store_result(ir_builder->CreateICmp(*p, lhs_value, rhs_value));
        }
    }

    void codegen::evaluate_pointer_math(ast::binary_expr & binary_expr, llvm::Value * lhs_value,
                                        llvm::Value * rhs_value) {

        assert((lhs_value->getType()->isPointerTy() and rhs_value->getType()->isIntegerTy())
               or (lhs_value->getType()->isIntegerTy() and rhs_value->getType()->isPointerTy()));

        auto * pointer = lhs_value->getType()->isPointerTy() ? lhs_value : rhs_value;
        auto * index = lhs_value->getType()->isPointerTy() ? rhs_value : lhs_value;

        auto * pointer_type = llvm::dyn_cast_or_null<llvm::PointerType>(pointer->getType());
        assert(pointer_type != nullptr);
        auto * element_ty = pointer_type->getPointerElementType();

        using operand = ast::binary_expr::operand;

        switch (binary_expr.op) {
        case operand::add:
        case operand::sub:
            return store_result(ir_builder->CreateGEP(element_ty, pointer, index));
        default:
            printError(binary_expr.location(), tok_to_string(binary_expr.op),
                       " is not implemented for pointers");
            assert(false);
        }
    }

    void codegen::evaluate_short_circuit(ast::binary_expr & binary_expr, llvm::Value * lhs_value) {

        auto * lhs_block = ir_builder->GetInsertBlock();
        auto * current_function = lhs_block->getParent();
        auto * rhs_block = llvm::BasicBlock::Create(context, "", current_function);

        auto * merge_block = llvm::BasicBlock::Create(context, "", current_function);

        // Check that this is an acutal short circuit
        // short on false if and-ing, short on true if or-ing
        using operand = ast::binary_expr::operand;
        assert(binary_expr.op == operand::bool_and or binary_expr.op == operand::bool_or);

        // if (true && rhs) -> should eval rhs
        auto * on_true = binary_expr.op == operand::bool_and ? rhs_block : merge_block;

        // if (false || rhs) -> should eval rhs
        auto * on_false = binary_expr.op == operand::bool_or ? rhs_block : merge_block;
        assert(on_true != on_false);

        ir_builder->CreateCondBr(lhs_value, on_true, on_false);

        ir_builder->SetInsertPoint(rhs_block);
        auto * rhs_value = get_value(*binary_expr.rhs, *this);
        ir_builder->CreateBr(merge_block);

        ir_builder->SetInsertPoint(merge_block);
        auto * phi = ir_builder->CreatePHI(lhs_value->getType(), 2);
        phi->addIncoming(lhs_value, lhs_block);
        phi->addIncoming(rhs_value, rhs_block);

        store_result(phi);
    }

    void codegen::verify_module() const { llvm::verifyModule(*ir_module, &llvm::errs()); }

    template<class... arg_t>
    [[noreturn]] void codegen::printError(std::optional<Location> loc,
                                          const arg_t &... args) const noexcept {
        std::stringstream to_print;

        if (loc.has_value()) { to_print << *loc << " : ICE :"; }

        (to_print << ... << args);

        context.emitError(to_print.str());

        // TODO: Better recovery
        assert(false);
    }

    void codegen::arg_at(ast::func_call_data & data) {

        auto * raw_arg_vector = find_alive_value("argv", false);
        auto * llvm_str_ty = llvm::Type::getInt8PtrTy(context);

        if (raw_arg_vector == nullptr) {
            auto * new_raw_arg_vector = new llvm::GlobalVariable{
                *ir_module, llvm_str_ty->getPointerTo(),
                true,       llvm::GlobalValue::LinkageTypes::ExternalLinkage,
                nullptr,    "argv"};
            new_raw_arg_vector->setExternallyInitialized(true);

            active_values.add_to_root("argv", new_raw_arg_vector);
            raw_arg_vector = new_raw_arg_vector;
        }

        assert(data.args_count() == 1);

        auto * index = get_value(data.arg(0), *this);
        auto * ptr_to_index = ir_builder->CreateGEP(
            llvm_str_ty, ir_builder->CreateLoad(llvm_str_ty->getPointerTo(), raw_arg_vector),
            index);

        store_result(ir_builder->CreateLoad(llvm_str_ty, ptr_to_index));
    }

    void codegen::arg_count(ast::func_call_data & /*unused*/) {

        auto * raw_arg_count = find_alive_value("argc", false);
        auto * llvm_i64_ty = llvm::Type::getInt64Ty(context);

        if (raw_arg_count == nullptr) {
            auto * new_raw_arg_count = new llvm::GlobalVariable{
                *ir_module, llvm_i64_ty, true, llvm::GlobalValue::LinkageTypes::ExternalLinkage,
                nullptr,    "argc"};
            new_raw_arg_count->setExternallyInitialized(true);

            active_values.add_to_root("argc", new_raw_arg_count);
            raw_arg_count = new_raw_arg_count;
        }
        auto * long_argc = ir_builder->CreateLoad(llvm_i64_ty, raw_arg_count);
        store_result(ir_builder->CreateTrunc(long_argc, find_type(ast::prim_type::int32)));
    }

    void codegen::syscall(ast::func_call_data & func_call_data) {
        std::vector<llvm::Value *> args;
        args.reserve(func_call_data.args_count());

        for (auto i = 0U; i < func_call_data.args_count(); ++i) {
            args.push_back(get_value(func_call_data.arg(i), *this));
        }

        static const auto syscall_constraints = generate_syscall_constraints_array();

        const auto & constraint = syscall_constraints.at(func_call_data.args_count() - 1);

        std::vector<llvm::Type *> param_types;
        param_types.reserve(args.size());
        for (auto * val : args) { param_types.push_back(val->getType()); }

        auto * func_type
            = llvm::FunctionType::get(find_type(ast::prim_type::int32), param_types, false);

        assert(llvm::InlineAsm::Verify(func_type, constraint));

        store_result(ir_builder->CreateCall(
            func_type, llvm::InlineAsm::get(func_type, "syscall", constraint, true), args));
    }

    void codegen::visit(ast::binary_expr & binary_expr) {

        auto * lhs_value = get_value(*binary_expr.lhs, *this);
        if (binary_expr.is_shortcircuiting()) {
            return evaluate_short_circuit(binary_expr, lhs_value);
        }

        if (binary_expr.op == ast::binary_expr::operand::member_access) {

            assert(lhs_value->getType()->isPointerTy());

            auto ast_struct_type
                = std::dynamic_pointer_cast<ast::struct_type>(binary_expr.lhs->type);
            assert(ast_struct_type != nullptr);

            auto * llvm_struct_type = find_type(ast_struct_type);
            assert(llvm_struct_type != nullptr);
            assert(llvm::dyn_cast<llvm::PointerType>(lhs_value->getType())
                       ->isOpaqueOrPointeeTypeMatches(llvm_struct_type));

            auto * field_node = dynamic_cast<ast::user_val *>(binary_expr.rhs.get());
            assert(field_node != nullptr);

            llvm::Type * result_type = nullptr;
            auto index = UINT64_MAX;
            for (auto i = 0U; i < ast_struct_type->field_count(); ++i) {
                if (const auto & [name, type] = ast_struct_type->field(i);
                    name == field_node->val) {
                    result_type = find_type(type);
                    index = i;
                    break;
                }
            }

            assert(result_type != nullptr);
            assert(index != UINT64_MAX);

            auto * elem_ptr = ir_builder->CreateStructGEP(llvm_struct_type, lhs_value, index);
            return store_result(ir_builder->CreateLoad(result_type, elem_ptr));
        }

        // We will generate here, as every expression after will need the rhs
        auto * rhs_value = get_value(*binary_expr.rhs, *this);

        const bool is_constant
            = llvm::isa<llvm::Constant>(lhs_value) and llvm::isa<llvm::Constant>(rhs_value);

        const bool is_float = lhs_value->getType()->isFloatingPointTy()
                           or rhs_value->getType()->isFloatingPointTy();

        if (binary_expr.is_comparison()) {
            return evaluate_comparison(binary_expr, lhs_value, rhs_value, is_float, is_constant);
        }

        // Specific for pointers
        if (lhs_value->getType()->isPointerTy() or rhs_value->getType()->isPointerTy()) {
            return evaluate_pointer_math(binary_expr, lhs_value, rhs_value);
        }

        // all other binary expressions

        using operand = ast::binary_expr::operand;
        using bin_ops = llvm::Instruction::BinaryOps;
        std::optional<bin_ops> bin_op;

        auto int_or_float = [&is_float](bin_ops int_pred, bin_ops float_pred) {
            return is_float ? float_pred : int_pred;
        };

        switch (binary_expr.op) {
        case operand::add:
            bin_op = int_or_float(bin_ops::Add, bin_ops::FAdd);
            break;
        case operand::sub:
            bin_op = int_or_float(bin_ops::Sub, bin_ops::FSub);
            break;
        case operand::mult:
            bin_op = int_or_float(bin_ops::Mul, bin_ops::FMul);
            break;
        case operand::div:
            bin_op = int_or_float(bin_ops::SDiv, bin_ops::FDiv);
            break;
        default:
            printError(binary_expr.location(), "Binary operator ", tok_to_string(binary_expr.op),
                       " is not implemented yet");
        }

        assert(bin_op.has_value());
        if (is_constant) {
            auto * constant_lhs = llvm::dyn_cast<llvm::Constant>(lhs_value);
            auto * constant_rhs = llvm::dyn_cast<llvm::Constant>(rhs_value);
            store_result(llvm::ConstantExpr::get(*bin_op, constant_lhs, constant_rhs));
        } else {
            store_result(ir_builder->CreateBinOp(*bin_op, lhs_value, rhs_value));
        }
    }

    void codegen::visit(ast::const_decl & const_decl) {

        auto * value = llvm::dyn_cast<llvm::Constant>(get_value(*const_decl.expr, *this));

        if (value == nullptr) {
            printError(const_decl.location(), const_decl.name_and_type.name(),
                       " is not a constant expression");
        }

        const auto linkage = const_decl.exported()
                               ? llvm::GlobalVariable::LinkageTypes::ExternalLinkage
                               : llvm::GlobalVariable::LinkageTypes::InternalLinkage;

        auto * global = new llvm::GlobalVariable{
            *ir_module, value->getType(), true, linkage, value, const_decl.name_and_type.name()};

        active_values.add_to_current_scope(const_decl.name_and_type.name(), global);
        if (const_decl.exported()) {
            program_globals.add(ir_module->getModuleIdentifier(), const_decl.name_and_type.name(),
                                global);
        }
    }

    void codegen::visit(ast::expr & expr) { expr.accept(*this); }

    void codegen::visit(ast::func_call_data & func_call_data) {
        const auto & func_name = func_call_data.name();

        if (auto iter = instrinics.find(func_name); iter != instrinics.end()) {
            return (this->*iter->second)(func_call_data);
        }

        auto * func = llvm::dyn_cast_or_null<llvm::Function>(find_alive_value(func_name));
        // TODO: Store the location on the func_call_data somehow
        if (func == nullptr) { printError(std::nullopt, func_name, " is not a function"); }

        auto * func_type = func->getFunctionType();

        std::vector<llvm::Value *> args;
        args.reserve(func_call_data.args_count());

        for (auto i = 0U; i < func_call_data.args_count(); ++i) {
            args.push_back(get_value(func_call_data.arg(i), *this));
        }

        store_result(ir_builder->CreateCall(func_type, func, args));
    }

    void codegen::visit(ast::func_call_expr & func_call_expr) {
        // TODO: Add accept to some other classes
        visit(func_call_expr.data);
    }

    void codegen::visit(ast::func_call_stmt & func_call_stmt) {
        visit(func_call_stmt.data);
        // NOTE: the returned value is dropped in this case
        drop_result();
    }

    void codegen::visit(ast::func_decl & func_decl) {

        std::vector<llvm::Type *> param_types;
        const auto param_count = func_decl.head.param_count();
        param_types.reserve(param_count);

        for (auto i = 0U; i < param_count; ++i) {

            auto param = func_decl.head.arg(i);
            auto * llvm_param_type = find_type(param.type(), param.location());
            if (dynamic_cast<ast::struct_type *>(param.type().get()) != nullptr) {
                // All structs need to be passed as pointers
                llvm_param_type = llvm_param_type->getPointerTo();
            }
            param_types.push_back(llvm_param_type);
        }

        auto * func_type = llvm::FunctionType::get(
            find_type(func_decl.head.ret_type(), func_decl.location()), param_types, false);

        // The only functions that need ExternalLinkage are "main" or exported ones
        auto linkage = (func_decl.head.name() == "main" or func_decl.exported())
                         ? llvm::Function::ExternalLinkage
                         : llvm::Function::InternalLinkage;

        auto * func
            = llvm::Function::Create(func_type, linkage, func_decl.head.name(), ir_module.get());

        // add the function to the current scope
        active_values.add_to_current_scope(func_decl.head.name(), func);

        // enter the function
        {
            auto & func_scope = active_values.add_scope();
            for (auto i = 0U; i < param_count; ++i) {
                const auto & name = func_decl.head.arg(i).name();
                auto * arg = func->getArg(i);
                arg->setName(name);
                func_scope.emplace(name, arg);
            }

            auto * block = llvm::BasicBlock::Create(context, func->getName(), func);
            ir_builder->SetInsertPoint(block);

            func_decl.body->accept(*this);

            // leave the function
            active_values.remove_scope();
        }

        // Ensure termination for the whole function
        // TODO: Iterate every block and check for termination on each?
        if (auto & last_bb = func->getBasicBlockList().back(); not last_bb.back().isTerminator()) {
            ir_builder->SetInsertPoint(&last_bb);
            if (auto * return_type = func_type->getReturnType(); return_type->isVoidTy()) {
                ir_builder->CreateRetVoid();
            } else if (func_decl.head.name() == "main" and return_type->isIntegerTy()) {
                // Like C++, we add a `return 0` at the end.
                ir_builder->CreateRet(llvm::ConstantInt::get(return_type, 0));
            } else {
                printError(func_decl.location(), "Function ", func_decl.head.name(),
                           " does not return a value at the end");
            }
        }

        if (func_decl.exported()) {
            program_globals.add(ir_module->getModuleIdentifier(), func_decl.head.name(), func);
        }
    }

    void codegen::visit(ast::if_expr & if_expr) {

        // Evaluate the condition
        llvm::Value * condition = get_value(*if_expr.condition, *this);

        // Optimize if the condition is constant.
        if (auto * constant_condition = llvm::dyn_cast<llvm::Constant>(condition);
            constant_condition != nullptr) {
            if (constant_condition == llvm::ConstantInt::getTrue(context)) {
                store_result(get_value(*if_expr.then_case, *this));
                return;
            }
            store_result(get_value(*if_expr.else_case, *this));
            return;
        }

        auto * start_block = ir_builder->GetInsertBlock();
        auto * current_function = start_block->getParent();

        // Generate the true branch
        auto * then_block = llvm::BasicBlock::Create(context, "", current_function);
        ir_builder->SetInsertPoint(then_block);
        auto * then_value = get_value(*if_expr.then_case, *this);

        auto * end_then = ir_builder->GetInsertBlock();

        // Generate the else block
        auto * else_block = llvm::BasicBlock::Create(context, "", current_function);
        ir_builder->SetInsertPoint(else_block);
        auto * else_value = get_value(*if_expr.else_case, *this);

        auto * end_else = ir_builder->GetInsertBlock();

        ir_builder->SetInsertPoint(start_block);
        ir_builder->CreateCondBr(condition, then_block, else_block);

        auto terminated
            = [](const llvm::BasicBlock * block) -> bool { return block->back().isTerminator(); };

        // merge the expressions
        auto * merge_block = llvm::BasicBlock::Create(context, "", current_function);
        if (not terminated(end_then)) {
            ir_builder->SetInsertPoint(end_then);
            ir_builder->CreateBr(merge_block);
        }

        if (not terminated(end_else)) {
            ir_builder->SetInsertPoint(end_else);
            ir_builder->CreateBr(merge_block);
        }

        ir_builder->SetInsertPoint(merge_block);
        auto * phi = ir_builder->CreatePHI(then_value->getType(), 2);
        phi->addIncoming(then_value, end_then);
        phi->addIncoming(else_value, end_else);
        store_result(phi);
    }

    void codegen::visit(ast::if_stmt & if_stmt) {

        // Evaluate the condition
        llvm::Value * condition = get_value(*if_stmt.condition, *this);

        auto * start_block = ir_builder->GetInsertBlock();
        auto * current_function = start_block->getParent();

        // Generate the true branch
        auto * then_block = llvm::BasicBlock::Create(context, "", current_function);
        ir_builder->SetInsertPoint(then_block);
        if_stmt.true_branch->accept(*this);

        auto * end_true = ir_builder->GetInsertBlock();

        auto terminated
            = [](const llvm::BasicBlock * block) -> bool { return block->back().isTerminator(); };

        if (if_stmt.else_branch == nullptr) {

            // Short cut to just merging
            auto * merge_block = llvm::BasicBlock::Create(context, "", current_function);

            if (not terminated(start_block)) {
                ir_builder->SetInsertPoint(start_block);
                ir_builder->CreateCondBr(condition, then_block, merge_block);
            }

            if (not terminated(end_true)) {
                ir_builder->SetInsertPoint(end_true);
                ir_builder->CreateBr(merge_block);
            }

            ir_builder->SetInsertPoint(merge_block);

            return;
        }

        // Generate the else block
        auto * else_block = llvm::BasicBlock::Create(context, "", current_function);
        ir_builder->SetInsertPoint(else_block);
        if_stmt.else_branch->accept(*this);

        auto * end_else = ir_builder->GetInsertBlock();

        ir_builder->SetInsertPoint(start_block);
        ir_builder->CreateCondBr(condition, then_block, else_block);

        // Ensure termination
        if (not terminated(end_else) and not terminated(end_true)) {
            auto * merge_block = llvm::BasicBlock::Create(context, "", current_function);
            if (not terminated(end_true)) {
                ir_builder->SetInsertPoint(end_true);
                ir_builder->CreateBr(merge_block);
            }

            if (not terminated(end_else)) {
                ir_builder->SetInsertPoint(end_else);
                ir_builder->CreateBr(merge_block);
            }

            ir_builder->SetInsertPoint(merge_block);
        }
    }

    void codegen::visit(ast::let_stmt & let_stmt) {

        auto * value = get_value(*let_stmt.value, *this);
        value->setName(let_stmt.name_and_type.name());
        active_values.add_to_current_scope(let_stmt.name_and_type.name(), value);
    }

    void codegen::visit(ast::node & node) { node.accept(*this); }

    void codegen::visit(ast::return_stmt & return_stmt) {
        if (return_stmt.value == nullptr) {
            ir_builder->CreateRetVoid();
            return;
        }

        auto * ret_val = get_value(*return_stmt.value, *this);
        ir_builder->CreateRet(ret_val);
    }

    void codegen::visit(ast::stmt & stmt) { stmt.accept(*this); }

    void codegen::visit(ast::stmt_sequence & stmt_sequence) {
        for (auto & stmt : stmt_sequence.stmts) { stmt->accept(*this); }
    }

    void codegen::visit(ast::struct_decl & struct_decl) {
        find_type(struct_decl.type(ir_module->getModuleIdentifier()), struct_decl.location());
    }

    void codegen::visit(ast::struct_init & struct_init) {

        auto ast_struct_type = std::dynamic_pointer_cast<ast::struct_type>(struct_init.type);
        assert(ast_struct_type != nullptr);

        auto * llvm_struct_type = find_type(ast_struct_type);
        assert(llvm_struct_type != nullptr);

        // Allocate the struct
        auto * struct_ptr = ir_builder->CreateAlloca(llvm_struct_type);

        // Initialize each field
        for (auto & [name, value] : struct_init.initializers) {

            auto index = UINT64_MAX;
            for (auto i = 0U; i < ast_struct_type->field_count(); ++i) {
                if (ast_struct_type->field(i).first == name) {
                    index = i;
                    break;
                }
            }
            assert(index != UINT64_MAX);

            auto * llvm_value = get_value(*value, *this);
            auto * field_ptr = ir_builder->CreateStructGEP(llvm_struct_type, struct_ptr, index);
            ir_builder->CreateStore(llvm_value, field_ptr);
        }

        return store_result(struct_ptr);
    }

    void codegen::visit(ast::top_level & top_level) { top_level.accept(*this); }

    void codegen::visit(ast::top_level_sequence & top_level_sequence) {

        if (not top_level_sequence.imports.empty()) {
            for (auto & [src_module, ids] : top_level_sequence.imports) {
                if (src_module == "env") { continue; }
                for (auto & id : ids) {
                    auto * global = program_globals.lookup(src_module, id);
                    if (global == nullptr) {
                        llvm::outs() << "Could not find " << id << " in exports of module "
                                     << src_module << '\n';
                        assert(false);
                    }
                    if (auto * func_orig = llvm::dyn_cast<llvm::Function>(global);
                        func_orig != nullptr) {
                        auto * func = llvm::Function::Create(func_orig->getFunctionType(),
                                                             llvm::GlobalValue::ExternalLinkage, id,
                                                             ir_module.get());
                        func->deleteBody();
                        active_values.add_to_current_scope(id, func);
                    } else if (auto * global_var = llvm::dyn_cast<llvm::GlobalVariable>(global);
                               global_var != nullptr) {
                        // must be a global constant
                        active_values.add_to_current_scope(id, global_var);
                    }
                }
            }
        }

        for (auto & item : top_level_sequence.items) {
            assert(item != nullptr);
            item->accept(*this);
        }
    }

    void codegen::visit(ast::typed_identifier & /*typed_identifier*/) {
        printError(std::nullopt, "In unimplemented function for typed_identifier");
    }

    void codegen::visit(ast::unary_expr & unary_expr) {
        auto * value = get_value(*unary_expr.expr, *this);
        auto * const_val = llvm::dyn_cast<llvm::Constant>(value);

        using operand = ast::unary_expr::operand;
        switch (unary_expr.op) {
        case operand::bool_not:
            if (const_val != nullptr) {
                store_result(llvm::ConstantExpr::getNot(const_val));
            } else {
                store_result(ir_builder->CreateNot(value));
            }
            break;
        case operand::deref: {
            assert(const_val == nullptr);

            auto * ptr_ty = llvm::dyn_cast<llvm::PointerType>(value->getType());
            assert(ptr_ty != nullptr);

            // TODO: Align it?
            store_result(ir_builder->CreateLoad(ptr_ty->getPointerElementType(), value));
        } break;
        case operand::negate:
            if (auto float_op = value->getType()->isFloatingPointTy(); const_val != nullptr) {
                store_result(float_op ? llvm::ConstantExpr::getFNeg(const_val)
                                      : llvm::ConstantExpr::getNeg(const_val));
            } else {
                store_result(float_op ? ir_builder->CreateFNeg(value)
                                      : ir_builder->CreateNeg(value));
            }
            break;
        }
    }

    void codegen::visit(ast::user_val & user_val) {
        using value_type = ast::user_val::value_type;
        switch (user_val.val_type) {
        case value_type::identifier: {
            auto * value = find_alive_value(user_val.val);
            if (value == nullptr) {
                printError(user_val.location(), "Could not find variable ", user_val.val);
            }
            return store_result(value);
        }
        case value_type::null: {
            auto & type = user_val.type;
            assert(type->is_pointer_type());
            auto * llvm_type = type_context.lower_to_llvm(type);
            assert(llvm_type != nullptr);
            return store_result(llvm::Constant::getNullValue(llvm_type));
        }
        case value_type::integer: {
            static constexpr auto hex_base = 16;
            static constexpr auto dec_base = 10;
            auto base = user_val.val.find_first_of('x') != std::string::npos ? hex_base : dec_base;
            // TODO: Get the type from the type_context
            return store_result(
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), user_val.val, base));
        }
        case value_type::floating:
            printError(user_val.location(), "Floating point IR not implemented");
            break;
        case value_type::character:
            switch (user_val.val.size()) {
            case 3:
                return store_result(
                    llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), user_val.val.at(1)));
            case 4:
                assert(user_val.val.at(1) == '\\');
                switch (user_val.val.at(2)) {
                case '0':
                    return store_result(
                        llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), '\0'));
                case 'n':
                    return store_result(
                        llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), '\n'));
                }
                [[fallthrough]];
            default:
                llvm::outs() << user_val.val << " cannot be interpreted as a character.\n";
                assert(false);
            }
        case value_type::boolean: {
            auto iter = valid_bools.find(user_val.val);
            assert(iter != valid_bools.end());
            return store_result(llvm::ConstantInt::getBool(context, iter->second));
        }
        case value_type::string: {
            assert(user_val.val.size() > 2);
            auto value = user_val.val.substr(1, user_val.val.size() - 2);
            return store_result(ir_builder->CreateGlobalStringPtr(value));
        }
        }
    }
} // namespace visitor
