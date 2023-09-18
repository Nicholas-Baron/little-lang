#pragma once

#include "location.hpp"

#include <deque>
#include <filesystem>
#include <memory> // unique_ptr
#include <optional>
#include <string>

#include <move_copy.hpp>

class lexer final {
  public:
    // `from_file` loads a file from given filename and uses that as input.
    // The file data is internally allocated and freed by the lexer.
    static std::unique_ptr<lexer> from_file(const std::string & filename,
                                            const std::filesystem::path & project_root);

    // `from_buffer` uses the given C string as its input.
    // Note that the lexer does not own the string and maintains a readonly view into it.
    // The caller must store the string *and* ensure that it is not modified while the lexer is
    // alive.
    static std::unique_ptr<lexer> from_buffer(const char * buffer, size_t size);

    non_copyable(lexer);
    non_movable(lexer);

    ~lexer() noexcept;

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
        as,
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
        dot,
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

    [[nodiscard]] bool has_more_tokens() { return peek_token() != token_type::eof; }

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
    bool next_chars(const std::string & text, unsigned offset = 0);

    [[nodiscard]] std::string file_name() const { return filename.value_or("internal buffer"); }

    [[nodiscard]] std::string module_name() const {
        if (project_root.has_value()) {
            return std::filesystem::relative(file_name(), *project_root);
        }
        return file_name();
    }

  private:
    lexer(const std::string & filename, const char * data, size_t size,
          const std::filesystem::path & project_root);
    lexer(const char * data, size_t size);

    template<class... args_t>
    void print_error(args_t... args) const;

    // `next_token` has some complex, yet modular, logic for determining a token's type.
    // These 3 functions are helpers to `next_token` that handle some, but not all, tokens each.
    token next_identifier(Location loc);
    token next_number(Location loc);
    token next_symbol(Location loc);

    // Helper to handle escape sequences
    char next_escaped();

    std::deque<token> peeked_tokens;
    std::optional<std::filesystem::path> filename;

    // XXX: Separation of responsibilities issue?
    std::optional<std::filesystem::path> project_root;

    const char * data;
    size_t length;
    size_t current_pos{0};

    int line_num{1};
    int col_num{0};

    // This enum is used to determine whether the parser came from a file or an internal buffer,
    // and so how it should be cleaned up.
    enum data_type { mmapped, read_buffer } type;
};

using lex_ptr = std::unique_ptr<lexer>;
