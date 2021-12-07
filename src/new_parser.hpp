#ifndef PARSER_HPP
#define PARSER_HPP

#include <ast/base_nodes.hpp>
#include <ast/nodes_forward.hpp>
#include <utils/move_copy.hpp>

#include <memory>
#include <optional>
#include <string>

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
    std::unique_ptr<ast::func_decl> parse_function();
    ast::stmt_ptr parse_statement();
    ast::stmt_ptr parse_compound_statement();

    enum class token_type {
        unknown,
        identifier,
        integer,
        floating,
        from,
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
        eof,
    };

    std::pair<token_type, std::string> peek_token() {
        if (not peeked_token.has_value()) { peeked_token = next_token(); }
        return peeked_token.value();
    }
    std::pair<token_type, std::string> next_token();

    char next_char();
    char peek_char(unsigned offset = 0);

#ifdef PARSER_TEST
  private:
#endif

    // helpers for next_token
    std::pair<token_type, std::string> next_identifier();
    std::pair<token_type, std::string> next_number();
    std::pair<token_type, std::string> next_symbol();

    std::optional<std::pair<token_type, std::string>> peeked_token;
    std::string filename;
    std::string error;

    const char * data;
    size_t length;
    size_t current_pos{0};
    enum data_type { mmapped, read_buffer } type;
};

#endif
