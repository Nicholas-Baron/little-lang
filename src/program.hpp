#ifndef PROGRAM_HPP
#define PROGRAM_HPP

#include "ast/top_lvl_nodes.hpp"
#include "settings.hpp"
#include "type_context.hpp"
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>

#include <optional>
#include <vector>

class program final {
  public:
    static std::optional<program> from_modules(const std::string & root_file,
                                               std::vector<ast::top_level_sequence> && modules,
                                               std::shared_ptr<Settings> settings);

    [[nodiscard]] bool type_check();
    void generate_ir();
    void emit_and_link();
    [[nodiscard]] int jit();

  private:
    program(std::vector<ast::top_level_sequence> && modules, std::shared_ptr<Settings> settings,
            std::string && project_root);

    std::string project_root;
    std::unique_ptr<llvm::LLVMContext> context;
    std::shared_ptr<Settings> settings;
    std::vector<ast::top_level_sequence> ast_modules;
    std::vector<std::unique_ptr<llvm::Module>> ir_modules;
    type_context typ_context;
};

#endif
