#ifndef PARSER_HPP
#define PARSER_HPP

#include "location.hpp"
#include <ast/base_nodes.hpp>
#include <ast/node_utils.hpp>
#include <ast/nodes_forward.hpp>
#include <ast/type.hpp>
#include <utils/move_copy.hpp>

#include <deque>
#include <map>
#include <memory> // unique_ptr
#include <optional>
#include <string>
#include <vector>

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
    static std::unique_ptr<parser> from_file(const std::string & filename);

    // `from_buffer` uses the given string as its input.
    // Note that the parser does not own the string and maintains a readonly view into it.
    // The caller must store the string *and* ensure that it is not modified while the parser is
    // alive.
    static std::unique_ptr<parser> from_buffer(std::string & buffer);

    // `parse` parses a single module (one file).
    // Any failure in parsing results in a `nullptr`.
    // Otherwise, the AST of the parser's input is returned.
    [[nodiscard]] std::unique_ptr<ast::top_level_sequence> parse();

    // In the case that `parse` failed, `error_message` will provide a human readable error.
    [[nodiscard]] std::string error_message() const { return error; }

    // The parser's inner state is rather complex, so not moving or copying it is essential.
    non_copyable(parser);
    non_movable(parser);

    ~parser() noexcept;

  private:
    parser(std::string filename, const char * data, size_t);
    parser(const char * data, size_t);

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
    ast::expr_ptr parse_atom();

    // A function call can either be a statement or an expression.
    // To reduce code duplication, `parse_func_call` handles both.
    // In the expression case, we have already consumed the name, so we must pass it in.
    ast::func_call_data parse_func_call(std::optional<std::string> func_name = std::nullopt);

    // This enum marks the type of a given token.
    // This is done to speed up equality checking,
    // and to denote whole groups of tokens (e.g. identifiers, integers).
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
        then,
        else_,
        return_,
        let,
        const_,
        null,
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
        amp,
        question,
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

    // This struct represents a token and can be compared against both a `token_type` and a string.
    struct token {
        token_type type;
        std::string text;
        Location location;

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

    // The following functions are used to read and pop from the token stream.
    // `consume_if` will only pop the next token if it is of the given type.
    std::optional<std::string> consume_if(token_type tok_type) {
        if (peek_token() == tok_type) { return next_token().text; }
        return std::nullopt;
    }

    // `peek_token` returns the next token without removing it from the token stream.
    token peek_token(size_t offset = 0) {
        while (peeked_tokens.size() < offset + 1) { peeked_tokens.push_back(next_token(true)); }
        return peeked_tokens.at(offset);
    }

    // `next_token` returns the next token and removes it from the token stream.
    token next_token(bool increasing_lookahead = false);

    // The following are functions that operate on the character stream.
    // `next_char` returns the next character and removes it from the stream.
    char next_char();

    // `peek_char` returns the character at some offset into the stream,
    // without removing it from the stream.
    char peek_char(unsigned offset = 0);

    // `next_chars` checks that the characters at some offset into the stream are equal to the
    // provided string.
    bool next_chars(const std::string &, unsigned offset = 0);

#ifdef PARSER_TEST
  private:
#endif

    // `next_token` has some complex, yet modular, logic for determining a token's type.
    // These 3 functions are helpers to `next_token` that handle some, but not all, tokens each.
    token next_identifier(Location);
    token next_number(Location);
    token next_symbol(Location);

    std::deque<token> peeked_tokens;
    std::string filename;
    std::string error;

    const char * data;
    size_t length;
    size_t current_pos{0};

    int line_num{1};
    int col_num{0};

    // This enum is used to determine whether the parser came from a file or an internal buffer,
    // and so how it should be cleaned up.
    enum data_type { mmapped, read_buffer } type;
};

#endif
