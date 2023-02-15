#pragma once

#include "base_nodes.hpp"
#include "lexer.hpp"
#include "location.hpp"
#include "node_utils.hpp"
#include "nodes_forward.hpp"
#include "type.hpp"
#include "type_context.hpp"

#include <filesystem>
#include <map>
#include <memory> // unique_ptr
#include <optional>
#include <sstream>
#include <vector>

#include <move_copy.hpp>

// This class takes some character input and produces an abstract syntax tree (AST).
// In a development compilation, its public interface is rather small,
// consisting only of two static functions, two member functions, and a destructor.
// However, for testing purposes,
// a larger swath of internal parsing helpers is exposed to test each one of them independently.
//
// The two static functions serve as factory functions for the parser class,
// as creating the parser may fail.
class parser final {
  public:
    // `from_file` loads a file from given filename and uses that as input.
    // The file data is internally allocated and freed by the parser.
    static std::unique_ptr<parser> from_file(const std::string & filename,
                                             const std::filesystem::path & project_root,
                                             ast::type_context & ty_context);

    // `from_buffer` uses the given string as its input.
    // Note that the parser does not own the string and maintains a readonly view into it.
    // The caller must store the string *and* ensure that it is not modified while the parser is
    // alive.
    static std::unique_ptr<parser> from_buffer(std::string & buffer,
                                               ast::type_context & ty_context);

    // `parse` parses a single module (one file).
    // Any failure in parsing results in a `nullptr`.
    // Otherwise, the AST of the parser's input is returned.
    [[nodiscard]] std::unique_ptr<ast::top_level_sequence> parse();

    // In the case that `parse` failed, `error_message` will provide a human readable error.
    // To check that no error has occured in parsing,
    // ensure that the result of this function is empty.
    // Each error is a separate string in the vector to allow for nicer formatting.
    [[nodiscard]] const std::vector<std::string> & error_message() const { return error_printout; }

    // The parser's inner state is rather complex, so not moving or copying it is essential.
    non_copyable(parser);
    non_movable(parser);

    ~parser() noexcept = default;

  private:
    explicit parser(lex_ptr && lex, ast::type_context & ty_context,
                    std::optional<std::filesystem::path> project_root = std::nullopt)
        : project_root{std::move(project_root)}
        , lex{std::move(lex)}
        , ty_context{ty_context} {}

    // As stated above,
    // there are some internals which need to be tested independently of each other.
#ifdef PARSER_TEST
  public:
#endif

    // The following parsing functions deal with non-expression syntax,
    // that is syntax that does not map to some value at runtime.
    std::map<std::string, std::vector<std::string>> parse_imports();
    ast::top_lvl_ptr parse_top_level();
    std::vector<ast::top_lvl_ptr> parse_exports();
    std::unique_ptr<ast::func_decl> parse_function();
    std::unique_ptr<ast::const_decl> parse_const_decl();
    std::unique_ptr<ast::struct_decl> parse_struct_decl();
    ast::stmt_ptr parse_statement();
    ast::stmt_ptr parse_compound_statement();
    std::unique_ptr<ast::if_stmt> parse_if_statement();
    std::unique_ptr<ast::return_stmt> parse_return_statement();
    std::unique_ptr<ast::let_stmt> parse_let_statement();
    ast::typed_identifier parse_opt_typed_identifier();
    ast::typed_identifier parse_typed_identifier();
    ast::type_ptr parse_type();

    // The following parsing functions deal with expression syntax,
    // that is syntax that does map to some value at runtime.
    // They are arrange in roughly precedence order,
    // such that the expressions they parse contain the function below them.
    // This is the basis of a recursive decent parser.
    ast::expr_ptr parse_expression();
    ast::expr_ptr parse_if_expression();
    ast::expr_ptr parse_boolean_expression();
    ast::expr_ptr parse_comparison();
    ast::expr_ptr parse_additive();
    ast::expr_ptr parse_multiplicative();
    ast::expr_ptr parse_unary();
    ast::expr_ptr parse_member_access();
    ast::expr_ptr parse_atom();

    // A function call can either be a statement or an expression.
    // To reduce code duplication, `parse_func_call` handles both.
    // In the expression case, we have already consumed the name, so we must pass it in.
    ast::func_call_data parse_func_call(std::optional<lexer::token> func_name = std::nullopt);

    std::unique_ptr<ast::struct_init> parse_struct_init(std::string && type_name, Location loc);

#ifdef PARSER_TEST
    [[nodiscard]] bool is_eof() const { return lex->peek_token() == lexer::token_type::eof; }

  private:
#endif

    // XXX: Separation of responsibilities issue?
    std::optional<std::filesystem::path> project_root;

    [[nodiscard]] std::string module_name() const {
        if (project_root.has_value()) {
            return std::filesystem::relative(lex->file_name(), *project_root);
        }
        return lex->file_name();
    }

    lex_ptr lex;
    ast::type_context & ty_context;
    std::vector<std::string> error_printout;

    template<typename... Args>
    void print_error(Location loc, Args... args);
};
