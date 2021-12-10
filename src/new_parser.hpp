#ifndef PARSER_HPP
#define PARSER_HPP

#include <ast/base_nodes.hpp>
#include <ast/nodes_forward.hpp>
#include <utils/move_copy.hpp>

#include <map>
#include <memory>
#include <optional>
#include <string>
#include <vector>

class parser final {
  public:
    static std::unique_ptr<parser> from_file(const std::string & filename);

    // We do not own the string. The caller must store the string.
    static std::unique_ptr<parser> from_buffer(std::string & buffer);

    [[nodiscard]] std::unique_ptr<ast::top_level_sequence> parse();

    [[nodiscard]] std::string error_message() { return error; }

    non_copyable(parser);
    non_movable(parser);

    ~parser() noexcept;

  private:
    parser(std::string filename, const char * data, size_t);
    parser(const char * data, size_t);

#ifdef PARSER_TEST
  public:
#endif

    // parsing functions
    std::map<std::string, std::vector<std::string>> parse_imports();
    ast::top_lvl_ptr parse_top_level();
    std::vector<ast::top_lvl_ptr> parse_exports();
    std::unique_ptr<ast::func_decl> parse_function();
    std::unique_ptr<ast::const_decl> parse_const_decl();
    ast::stmt_ptr parse_statement();
    ast::stmt_ptr parse_compound_statement();
    std::unique_ptr<ast::if_stmt> parse_if_statement();
    std::unique_ptr<ast::return_stmt> parse_return_statement();
    std::unique_ptr<ast::let_stmt> parse_let_statement();
    std::string parse_type();

    // parse expressions
    ast::expr_ptr parse_expression();
    ast::expr_ptr parse_boolean_expression();
    ast::expr_ptr parse_comparison();
    ast::expr_ptr parse_additive();
    ast::expr_ptr parse_multiplicative();
    ast::expr_ptr parse_unary();
    ast::expr_ptr parse_atom();
    ast::func_call_data parse_func_call(std::optional<std::string> func_name = std::nullopt);

    enum class token_type {
        unknown,
        identifier,
        prim_type,
        integer,
        floating,
        from,
        import_,
        export_,
        if_,
        else_,
        return_,
        let,
        const_,
        character,
        string,
        boolean,
        // symbols
        lparen,
        rparen,
        lbrace,
        rbrace,
        arrow,
        comma,
        colon,
        equal,
        double_and,
        double_or,
        lt,
        le,
        gt,
        ge,
        eq,
        ne,
        plus,
        minus,
        percent,
        asterik,
        slash,
        exclam,
        semi,
        eof,
    };

    struct token {
        token_type type;
        std::string text;

      private:
        friend bool operator==(const token & tok, token_type type) { return tok.type == type; }
        friend bool operator==(const token & tok, const std::string & text) {
            return tok.text == text;
        }

        friend bool operator!=(const token & tok, token_type type) { return tok.type != type; }

        friend bool operator!=(const token & tok, const std::string & text) {
            return tok.text != text;
        }
    };

    std::optional<std::string> consume_if(token_type tok_type) {
        if (peek_token() == tok_type) { return next_token().text; }
        return std::nullopt;
    }
    token peek_token() {
        if (not peeked_token.has_value()) { peeked_token = next_token(); }
        return peeked_token.value();
    }
    token next_token();

    char next_char();
    char peek_char(unsigned offset = 0);

#ifdef PARSER_TEST
  private:
#endif

    // helpers for next_token
    token next_identifier();
    token next_number();
    token next_symbol();

    std::optional<token> peeked_token;
    std::string filename;
    std::string error;

    const char * data;
    size_t length;
    size_t current_pos{0};
    enum data_type { mmapped, read_buffer } type;
};

#endif
