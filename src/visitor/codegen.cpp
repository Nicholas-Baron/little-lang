#include "codegen.hpp"

#include "ast/expr_nodes.hpp"
#include "ast/nodes.hpp"
#include "ast/type.hpp"
#include "emit_asm.hpp"
#include "token_to_string.hpp"
#include "type_context.hpp"
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>

#include <sstream>

namespace visitor {

    namespace {

        const std::map<std::string, bool> valid_bools{{"true", true},   {"True", true},
                                                      {"TRUE", true},   {"false", false},
                                                      {"False", false}, {"FALSE", false}};

        namespace constraints {

            static constexpr size_t count_in(const char * text, char c) {
                auto count = 0U;
                while (text != nullptr and *text != '\0') {
                    count += static_cast<unsigned int>(*text == c);
                    ++text;
                }
                return count;
            }

            static constexpr auto * first_part = "=A,A,{di},{si},{dx},{r10},{r8},{r9},";
            static constexpr auto count = count_in(first_part, ',') - 1;
            static constexpr auto * suffix = ",~{r11},~{rcx},~{dirflag},~{fpsr},~{flags}";

        } // namespace constraints

        std::array<std::string, constraints::count> generate_syscall_constraints_array() {
            using namespace constraints;
            std::array<std::string, count> result;

            auto i = 0U;
            for (const auto * iter = strchr(first_part, ',') + 1; iter != nullptr and *iter != '\0';
                 ++iter) {

                if (*iter != ',') { continue; }

                auto constraints = std::string{first_part, static_cast<size_t>(iter - first_part)};

                result[i++] = constraints + suffix;
            }

            return result;
        }
    } // namespace

    codegen::codegen(const std::string & name, llvm::LLVMContext * context,
                     global_map<std::string, llvm::GlobalObject *> * program_globals,
                     class type_context * typ_context)
        : context{context}
        , ir_module{std::make_unique<llvm::Module>(name, *context)}
        , ir_builder{std::make_unique<llvm::IRBuilder<>>(*context)}
        , type_context(typ_context)
        , active_values{{}}
        , program_globals{program_globals}
        , instrinics{{"syscall", &codegen::syscall}} {
        ir_module->setTargetTriple(init_llvm_targets());
    }

    void codegen::dump() const { llvm::outs() << *ir_module << '\n'; }

    llvm::Value * codegen::find_alive_value(const std::string & name) const {
        // Walk backwards thru scopes
        for (auto scope = active_values.rbegin(); scope != active_values.rend(); ++scope) {
            if (auto iter = scope->find(name); iter != scope->end()) {

                // Globals are always pointers to data, so we should try to use the initializer
                auto * global = llvm::dyn_cast<llvm::GlobalVariable>(iter->second);
                if (global != nullptr and global->hasInitializer()) {
                    return global->getInitializer();
                }
                return iter->second;
            }
        }
        printError("Could not find value " + name);
        return nullptr;
    }

    llvm::Type * codegen::find_type(const ast::type & name, std::optional<Location> loc) {

        auto * typ = type_context->lower_to_llvm(name);
        if (typ == nullptr) {
            std::stringstream ss;
            ss << name << " is not a valid type?";
            printError(ss.str(), loc);
        }
        return typ;
    }

    void codegen::evaluate_comparison(ast::binary_expr & binary_expr, llvm::Value * lhs_value,
                                      llvm::Value * rhs_value, bool is_int, bool is_constant) {

        assert(lhs_value != nullptr or rhs_value != nullptr);
        using operand = ast::binary_expr::operand;

        if (lhs_value == nullptr or rhs_value == nullptr) {
            // Compare the other value to `null`.
            switch (auto * pointer = lhs_value == nullptr ? rhs_value : lhs_value; binary_expr.op) {
            case operand::eq:
                return store_result(ir_builder->CreateIsNull(pointer));
            case operand::ne:
                return store_result(ir_builder->CreateIsNotNull(pointer));
            default:
                printError("Comparison operator " + tok_to_string(binary_expr.op)
                               + " cannot work on pointers.",
                           binary_expr.location());
                assert(false);
            }
        }

        using predicate = llvm::CmpInst::Predicate;
        auto int_or_float = [&is_int](predicate int_pred, predicate float_pred) {
            return is_int ? int_pred : float_pred;
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
            printError("Comparison operator " + tok_to_string(binary_expr.op)
                           + " is not implemented yet",
                       binary_expr.location());
            assert(false);
        }
        assert(p.has_value());
        if (is_constant) {
            auto * constant_lhs = llvm::dyn_cast<llvm::Constant>(lhs_value);
            auto * constant_rhs = llvm::dyn_cast<llvm::Constant>(rhs_value);
            store_result(llvm::ConstantExpr::getCompare(*p, constant_lhs, constant_rhs));
        } else if (is_int) {
            store_result(ir_builder->CreateICmp(*p, lhs_value, rhs_value));
        } else {
            store_result(ir_builder->CreateFCmp(*p, lhs_value, rhs_value));
        }
    }

    void codegen::evaluate_short_circuit(ast::binary_expr & binary_expr, llvm::Value * lhs_value) {

        auto * lhs_block = ir_builder->GetInsertBlock();
        auto * current_function = lhs_block->getParent();
        auto * rhs_block = llvm::BasicBlock::Create(*context, "", current_function);

        auto * merge_block = llvm::BasicBlock::Create(*context, "", current_function);

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

    void codegen::printError(const std::string & name, std::optional<Location> loc) const {
        if (loc == std::nullopt) {
            context->emitError(name);
        } else {
            std::stringstream to_print;
            to_print << *loc << " : " << name;
            context->emitError(to_print.str());
        }
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
            = llvm::FunctionType::get(find_type(*ast::prim_type::int32), param_types, false);

        assert(llvm::InlineAsm::Verify(func_type, constraint));

        store_result(ir_builder->CreateCall(
            func_type, llvm::InlineAsm::get(func_type, "syscall", constraint, true), args));
    }

    void codegen::visit(ast::binary_expr & binary_expr) {

        auto * lhs_value = get_value(*binary_expr.lhs, *this);
        if (binary_expr.is_shortcircuiting()) {
            return evaluate_short_circuit(binary_expr, lhs_value);
        }

        // We will generate here, as every expression after will need the rhs
        auto * rhs_value = get_value(*binary_expr.rhs, *this);

        const bool is_constant
            = llvm::isa<llvm::Constant>(lhs_value) and llvm::isa<llvm::Constant>(rhs_value);

        const bool is_int
            = lhs_value->getType()->isIntegerTy() and rhs_value->getType()->isIntegerTy();

        if (binary_expr.is_comparison()) {
            return evaluate_comparison(binary_expr, lhs_value, rhs_value, is_int, is_constant);
        }

        using operand = ast::binary_expr::operand;

        // Specific for pointers
        if (lhs_value->getType()->isPointerTy() or rhs_value->getType()->isPointerTy()) {
            assert(
                (lhs_value->getType()->isPointerTy() and rhs_value->getType()->isIntegerTy())
                or (lhs_value->getType()->isIntegerTy() and rhs_value->getType()->isPointerTy()));

            auto * pointer = lhs_value->getType()->isPointerTy() ? lhs_value : rhs_value;
            auto * index = lhs_value->getType()->isPointerTy() ? rhs_value : lhs_value;
            auto * element_ty
                = llvm::dyn_cast<llvm::PointerType>(pointer->getType())->getElementType();

            switch (binary_expr.op) {
            case operand::add:
            case operand::sub:
                return store_result(ir_builder->CreateGEP(element_ty, pointer, index));
            default:
                printError(tok_to_string(binary_expr.op) + " is not implemented for pointers",
                           binary_expr.location());
                assert(false);
            }
        }

        // all other binary expressions

        using bin_ops = llvm::Instruction::BinaryOps;
        std::optional<bin_ops> bin_op;

        auto int_or_float = [&is_int](bin_ops int_pred, bin_ops float_pred) {
            return is_int ? int_pred : float_pred;
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
            printError("Binary operator " + tok_to_string(binary_expr.op)
                           + " is not implemented yet",
                       binary_expr.location());
            assert(false);
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
            printError(const_decl.name_and_type.name() + " is not a constant expression",
                       const_decl.location());
            assert(false);
        }

        auto * global = new llvm::GlobalVariable{
            *ir_module, value->getType(),
            true,       llvm::GlobalVariable::LinkageTypes::ExternalLinkage,
            value,      const_decl.name_and_type.name()};

        active_values.back().emplace(const_decl.name_and_type.name(), global);
        if (const_decl.exported()) {
            program_globals->add(ir_module->getModuleIdentifier(), const_decl.name_and_type.name(),
                                 global);
        }
    }

    void codegen::visit(ast::expr & expr) { expr.accept(*this); }

    void codegen::visit(ast::func_call_data & func_call_data) {
        const auto & func_name = func_call_data.name();

        if (auto iter = instrinics.find(func_name); iter != instrinics.end()) {
            return (this->*iter->second)(func_call_data);
        }

        auto * func_val = find_alive_value(func_name);

        // TODO: A better exit strategy
        if (func_val == nullptr) {
            printError("Could not find function named " + func_name);
            assert(false);
        }

        auto * func = llvm::dyn_cast<llvm::Function>(func_val);
        if (func == nullptr) {
            printError(func_name + " is not a function");
            assert(false);
        }

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
            param_types.push_back(find_type(*param.type(), param.location()));
        }

        auto * func_type = llvm::FunctionType::get(
            find_type(*func_decl.head.ret_type(), func_decl.location()), param_types, false);

        // The only functions that need ExternalLinkage are "main" or exported ones
        auto linkage = (func_decl.head.name() == "main" or func_decl.exported())
                         ? llvm::Function::ExternalLinkage
                         : llvm::Function::InternalLinkage;

        auto * func
            = llvm::Function::Create(func_type, linkage, func_decl.head.name(), ir_module.get());

        // add the function to the current scope
        active_values.back().emplace(func_decl.head.name(), func);

        // enter the function
        active_values.emplace_back();
        for (auto i = 0U; i < param_count; ++i) {
            const auto & name = func_decl.head.arg(i).name();
            auto * arg = func->getArg(i);
            arg->setName(name);
            active_values.back().emplace(name, arg);
        }

        auto * block = llvm::BasicBlock::Create(*context, func->getName(), func);
        ir_builder->SetInsertPoint(block);

        func_decl.body->accept(*this);

        // leave the function
        active_values.pop_back();

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
                printError("Function " + func_decl.head.name()
                           + " does not return a value at the end");
                assert(false);
            }
        }

        if (func_decl.exported()) {
            program_globals->add(ir_module->getModuleIdentifier(), func_decl.head.name(), func);
        }
    }

    void codegen::visit(ast::if_expr & if_expr) {

        // Evaluate the condition
        llvm::Value * condition = get_value(*if_expr.condition, *this);

        // Optimize if the condition is constant.
        if (auto * constant_condition = llvm::dyn_cast<llvm::Constant>(condition);
            constant_condition != nullptr) {
            if (constant_condition == llvm::ConstantInt::getTrue(*context)) {
                store_result(get_value(*if_expr.then_case, *this));
                return;
            }
            store_result(get_value(*if_expr.else_case, *this));
            return;
        }

        auto * start_block = ir_builder->GetInsertBlock();
        auto * current_function = start_block->getParent();

        // Generate the true branch
        auto * then_block = llvm::BasicBlock::Create(*context, "", current_function);
        ir_builder->SetInsertPoint(then_block);
        auto * then_value = get_value(*if_expr.then_case, *this);

        auto * end_then = ir_builder->GetInsertBlock();

        // Generate the else block
        auto * else_block = llvm::BasicBlock::Create(*context, "", current_function);
        ir_builder->SetInsertPoint(else_block);
        auto * else_value = get_value(*if_expr.else_case, *this);

        auto * end_else = ir_builder->GetInsertBlock();

        ir_builder->SetInsertPoint(start_block);
        ir_builder->CreateCondBr(condition, then_block, else_block);

        auto terminated
            = [](const llvm::BasicBlock * block) -> bool { return block->back().isTerminator(); };

        // merge the expressions
        auto * merge_block = llvm::BasicBlock::Create(*context, "", current_function);
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
        auto * then_block = llvm::BasicBlock::Create(*context, "", current_function);
        ir_builder->SetInsertPoint(then_block);
        if_stmt.true_branch->accept(*this);

        auto * end_true = ir_builder->GetInsertBlock();

        auto terminated
            = [](const llvm::BasicBlock * block) -> bool { return block->back().isTerminator(); };

        if (if_stmt.else_branch == nullptr) {

            // Short cut to just merging
            auto * merge_block = llvm::BasicBlock::Create(*context, "", current_function);

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
        auto * else_block = llvm::BasicBlock::Create(*context, "", current_function);
        ir_builder->SetInsertPoint(else_block);
        if_stmt.else_branch->accept(*this);

        auto * end_else = ir_builder->GetInsertBlock();

        ir_builder->SetInsertPoint(start_block);
        ir_builder->CreateCondBr(condition, then_block, else_block);

        // Ensure termination
        if (not terminated(end_else) and not terminated(end_true)) {
            auto * merge_block = llvm::BasicBlock::Create(*context, "", current_function);
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

        active_values.back().emplace(let_stmt.name_and_type.name(), value);
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

    void codegen::visit(ast::top_level & top_level) { top_level.accept(*this); }

    void codegen::visit(ast::top_level_sequence & top_level_sequence) {

        if (not top_level_sequence.imports.empty()) {
            assert(program_globals != nullptr);
            for (auto & [src_module, ids] : top_level_sequence.imports) {
                for (auto & id : ids) {
                    auto * global = program_globals->lookup(src_module, id);
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
                        active_values.back().emplace(id, func);
                    } else if (auto * global_var = llvm::dyn_cast<llvm::GlobalVariable>(global);
                               global_var != nullptr) {
                        // must be a global constant
                        active_values.back().emplace(id, global_var);
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
        printError("In unimplemented function for typed_identifier");
        assert(false);
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
            store_result(ir_builder->CreateLoad(ptr_ty->getElementType(), value));
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
        switch (user_val.type) {
        case value_type::identifier: {
            auto * value = find_alive_value(user_val.val);
            if (value == nullptr) {
                printError("Could not find variable " + user_val.val);
                // TODO: Better recovery
                assert(false);
            }
            return store_result(value);
        }
        case value_type::null:
            // TODO: Use extra type information to create an actual llvm null value
            return store_result(nullptr);
        case value_type::integer: {
            static constexpr auto hex_base = 16;
            static constexpr auto dec_base = 10;
            auto base = user_val.val.find_first_of('x') != std::string::npos ? hex_base : dec_base;
            // TODO: Get the type from the type_context
            return store_result(
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), user_val.val, base));
        }
        case value_type::floating:
            printError("Floating point IR not implemented");
            assert(false);
            break;
        case value_type::character:
            switch (user_val.val.size()) {
            case 3:
                return store_result(
                    llvm::ConstantInt::get(llvm::Type::getInt8Ty(*context), user_val.val.at(1)));
            case 4:
                assert(user_val.val.at(1) == '\\');
                switch (user_val.val.at(2)) {
                case '0':
                    return store_result(
                        llvm::ConstantInt::get(llvm::Type::getInt8Ty(*context), '\0'));
                case 'n':
                    return store_result(
                        llvm::ConstantInt::get(llvm::Type::getInt8Ty(*context), '\n'));
                }
                [[fallthrough]];
            default:
                llvm::outs() << user_val.val << " cannot be interpreted as a character.\n";
                assert(false);
            }
        case value_type::boolean: {
            auto iter = valid_bools.find(user_val.val);
            assert(iter != valid_bools.end());
            return store_result(llvm::ConstantInt::getBool(*context, iter->second));
        }
        case value_type::string: {
            assert(user_val.val.size() > 2);
            auto value = user_val.val.substr(1, user_val.val.size() - 2);
            return store_result(ir_builder->CreateGlobalStringPtr(value));
        }
        }
    }
} // namespace visitor
