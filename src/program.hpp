#ifndef PROGRAM_HPP
#define PROGRAM_HPP

#include "ast/top_lvl_nodes.hpp"
#include "ast/type_context.hpp"
#include "control_flow/graph.hpp"
#include "llvm_type_lowering.hpp"
#include "move_copy.hpp"
#include "settings.hpp"

#include <vector>

#include <llvm/IR/LLVMContext.h>

class program final {
  public:
    // Load all of the required files for one root file, which should have `main` in it.
    // Using a `unique_ptr` will allow `program` to be non-movable.
    static std::unique_ptr<program> from_root_file(const std::string & root_filename,
                                                   ast::type_context & ty_context,
                                                   std::shared_ptr<Settings> settings);

    void lower_to_cfg();
    [[nodiscard]] bool type_check();
    void generate_ir();

    // Returns the absolute path to the final program
    std::string emit_and_link();
    [[nodiscard]] uint64_t jit();

    non_copyable(program);

    // These need to be out of line for LLVM types
    program(program &&) noexcept;
    program & operator=(program &&) noexcept;
    ~program() noexcept;

  private:
    program(std::vector<ast::top_level_sequence> && modules, ast::type_context & ty_context,
            std::shared_ptr<Settings> settings, std::string && project_root);

    std::string project_root;
    std::unique_ptr<llvm::LLVMContext> context;
    std::shared_ptr<Settings> settings;
    std::vector<ast::top_level_sequence> ast_modules;
    std::unique_ptr<control_flow::graph> cfg;
    std::vector<std::unique_ptr<llvm::Module>> ir_modules;
    // HACK: ty_context needs to be a pointer to allow moves
    ast::type_context * ty_context;
    llvm_type_lowering llvm_lowering;
};

#endif
