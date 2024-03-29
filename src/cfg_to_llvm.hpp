#pragma once

#include "control_flow/node_forward.hpp"
#include "control_flow/visitor.hpp"
#include "move_copy.hpp"
#include "utils/global_map.hpp"
#include "utils/scoped_map.hpp"

#include <unordered_set>

// Forward declare LLVM stuff
namespace llvm {
    class GlobalObject;
    class LLVMContext;
    class IRBuilderBase;
    class Module;
    class Value;
} // namespace llvm

class cfg_to_llvm final : public control_flow::visitor {
  public:
    cfg_to_llvm(const std::string & name, llvm::LLVMContext & context,
                global_map<std::string, llvm::GlobalObject *> & globals,
                class llvm_type_lowering & typ_context);

    non_copyable(cfg_to_llvm);
    non_movable(cfg_to_llvm);

    ~cfg_to_llvm() noexcept override;

    void verify_module() const;
    void dump() const;

    [[nodiscard]] std::unique_ptr<llvm::Module> take_ir_module() && noexcept {
        return std::move(ir_module);
    }

  private:
    // clang-format off
#define expand_node_macro(name) void visit(control_flow::name & name) override;
        all_cfg_nodes
#undef expand_node_macro

    struct node_data {
        llvm::Value * value;
        llvm::BasicBlock * parent_block;
		// Using the ast type allows us to lower lazily.
		ast::type_ptr ast_type;

        node_data(llvm::IRBuilderBase & builder, llvm::Value * val, ast::type_ptr type);
    };

    // clang-format on

    void bind_value(const control_flow::node & node, llvm::Value * value, ast::type_ptr type);
    [[nodiscard]] const node_data * find_value_of(const control_flow::node * node) const;

    void arg_at(control_flow::intrinsic_call & intrinsic_call);
    void arg_count(control_flow::intrinsic_call & intrinsic_call);
    void syscall(control_flow::intrinsic_call & intrinsic_call);

    static void patch_parent_block(llvm::Function * current_function,
                                   std::vector<cfg_to_llvm::node_data> & values,
                                   node_data & incoming_value, llvm::BasicBlock * phi_block);
    // Keep these behind unique_ptr to allow for moving the visitor
    // context is not owned by us, but is sent to us via the constructor.
    llvm::LLVMContext & context;
    std::unique_ptr<llvm::Module> ir_module;
    std::unique_ptr<llvm::IRBuilderBase> ir_builder;

    global_map<std::string, llvm::GlobalObject *> globals;
    llvm_type_lowering & type_lowering;

    std::map<const control_flow::node *, node_data> values;
    std::map<std::string, void (cfg_to_llvm::*)(control_flow::intrinsic_call &)> intrinsics;
    std::unordered_set<const control_flow::node *> visited;
    scoped_map<std::string, llvm::Value *> local_names;
};
