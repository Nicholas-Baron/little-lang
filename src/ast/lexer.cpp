#include "lexer.hpp"

#include <cassert>
#include <cctype>     // isspace
#include <filesystem> // absolute
#include <iostream>   // cerr
#include <map>

#include <fcntl.h>    // open
#include <sys/mman.h> // mmap
#include <sys/stat.h> // fstat
#include <unistd.h>   // close

std::unique_ptr<lexer> lexer::from_file(const std::string & filename,
                                        const std::filesystem::path & project_root) {

    // First, we open the file via a Linux syscall.
    // NOLINTNEXTLINE (*-vararg)
    auto fd = open(filename.c_str(), O_CLOEXEC | O_RDONLY);
    if (fd == -1) {
        perror("lexer open");
        std::cerr << "Failed to open " << filename << std::endl;
        return nullptr;
    }

    // Then, we read the open file's size.
    struct stat file_stats {};

    if (fstat(fd, &file_stats) == -1) {
        perror("lexer stat");
        return nullptr;
    }

    const auto file_length = file_stats.st_size;

    // Next, we map the file into our memory.
    auto * data = static_cast<char *>(mmap(nullptr, file_length, PROT_READ, MAP_PRIVATE, fd, 0));
    if (data == MAP_FAILED) {
        perror("lexer mmap");
        return nullptr;
    }

    // Finally, we close that file, as the mapping does not need it open.
    if (close(fd) != 0) {
        perror("lexer close");
        return nullptr;
    }

    // NOTE: `make_unique` does not like private constructors.
    return std::unique_ptr<lexer>(new lexer(filename, data, file_length, project_root));
}

std::unique_ptr<lexer> lexer::from_buffer(const char * buffer, size_t size) {
    return std::unique_ptr<lexer>(new lexer(buffer, size));
}

lexer::lexer(const std::string & filename, const char * data, size_t size,
             const std::filesystem::path & project_root)
    : filename{std::filesystem::absolute(filename)}
    , project_root{project_root}
    , data{data}
    , length{size}
    , type{data_type::mmapped} {}

lexer::lexer(const char * data, size_t size)
    : filename{"internal buffer"}
    , data{data}
    , length{size}
    , type{data_type::read_buffer} {}

lexer::~lexer() noexcept {
    if (type == data_type::mmapped) {
        // If we mapped in a file for our input, we need to clean up that mapping.
        // Since we are being destroyed, we can mutate our member variables.
        // NOLINTNEXTLINE (*-const-cast)
        if (munmap(const_cast<char *>(data), length) == -1) { perror("lexer munmap"); }
    }
}

lexer::token lexer::next_token(bool increasing_lookahead) {

    if (not peeked_tokens.empty() and not increasing_lookahead) {
        // return the already processed token when one exists
        auto result = peeked_tokens.front();
        peeked_tokens.pop_front();
        return result;
    }

    while (true) {
        // go to first non-whitespace character
        while (isspace(peek_char()) != 0) { next_char(); }

        auto found_comment = (peek_char() == '/' and peek_char(1) == '/') or peek_char() == '#';
        if (not found_comment and tolower(peek_char()) == 'c') {
            found_comment = next_chars("omment", 1);
        }
        if (not found_comment) { break; }

        // Skip the comment line
        while (peek_char() != '\n' and peek_char() != '\r' and peek_char() != EOF) { next_char(); }

        if (peek_char() == EOF) { break; }
    }

    const Location loc{line_num, col_num};

    if (peek_char() == EOF) { return {token_type::eof, "", loc}; }

    // we are now at the first meaningful token
    if (isalpha(peek_char()) != 0 or peek_char() == '_') { return next_identifier(loc); }
    if (isdigit(peek_char()) != 0) { return next_number(loc); }
    return next_symbol(loc);
}

template<class... args_t>
void lexer::print_error(args_t... args) const {

    std::cerr << module_name() << ':' << line_num << ':' << col_num << ": ";
    (std::cerr << ... << args) << std::endl;
}

lexer::token lexer::next_identifier(Location loc) {

    std::string to_ret;

    // parse a word
    // XXX: this may be bad on performance
    while (isalnum(peek_char()) != 0 or peek_char() == '_') { to_ret += next_char(); }

    static const std::map<std::string, lexer::token_type> reserved_words{
  // alternate tokens
        {"and",    token_type::double_and},
        {"equals", token_type::eq        },
        {"is",     token_type::colon     },
        {"or",     token_type::double_or },
 // keywords
        {"const",  token_type::const_    },
        {"else",   token_type::else_     },
        {"export", token_type::export_   },
        {"from",   token_type::from      },
        {"if",     token_type::if_       },
        {"import", token_type::import_   },
        {"let",    token_type::let       },
        {"null",   token_type::null      },
        {"ret",    token_type::return_   },
        {"return", token_type::return_   },
        {"then",   token_type::then      },
        {"as",     token_type::as        },
 // primitive types
        {"bool",   token_type::prim_type },
        {"char",   token_type::prim_type },
        {"float",  token_type::prim_type },
        {"int8",   token_type::prim_type },
        {"int16",  token_type::prim_type },
        {"int32",  token_type::prim_type },
        {"int64",  token_type::prim_type },
        {"string", token_type::prim_type },
        {"unit",   token_type::prim_type },
 // literal values
        {"true",   token_type::boolean   },
        {"false",  token_type::boolean   },
    };

    if (auto iter = reserved_words.find(to_ret); iter != reserved_words.end()) {
        return {iter->second, std::move(to_ret), loc};
    }
    return {token_type::identifier, std::move(to_ret), loc};
}

lexer::token lexer::next_number(Location loc) {

    auto current_char = next_char();
    assert(isdigit(current_char));

    std::string to_ret;
    to_ret += current_char;

    if (current_char == '0') {
        // either we are hexadecimal or just 0
        if (tolower(peek_char()) == 'x') {
            // remove the 'x' or 'X'
            to_ret += next_char();
            // consume all hexadecimal digits
            while (isxdigit(peek_char()) != 0) { to_ret += next_char(); }
        }

        return {token_type::integer, std::move(to_ret), loc};
    }

    // known: the first digit is not 0

    while (isdigit(peek_char()) != 0) { to_ret += next_char(); }

    if (peek_char() != '.') { return {token_type::integer, to_ret, loc}; }

    // known: the next character is a '.'

    to_ret += next_char();

    while (isdigit(peek_char()) != 0) { to_ret += next_char(); }

    // TODO: Handle `1.0f`?

    if (to_ret.back() == '.') {
        print_error("Cannot end a floating point literal with a `.`");
        return {token_type::unknown, std::move(to_ret), loc};
    }

    if (to_ret.back() == '0') {
        print_error("Cannot end a floating point literal with a `0`");
        return {token_type::unknown, std::move(to_ret), loc};
    }

    return {token_type::floating, std::move(to_ret), loc};
}

char lexer::next_escaped() {
    auto current_char = next_char();
    switch (current_char) {
    case 'n':
        return '\n';
    case 'r':
        return '\r';
    case '0':
        return '\0';
    }
    print_error("Unknown escaped character: ", static_cast<unsigned>(current_char), " \'",
                current_char, "\'. Ignoring the `\\` for now.");
    return current_char;
}

lexer::token lexer::next_symbol(Location loc) {

    switch (const auto current_char = next_char(); current_char) {
    case EOF:
        return {token_type::eof, "", loc};
    case '(':
        return {token_type::lparen, "(", loc};
    case ')':
        return {token_type::rparen, ")", loc};
    case '{':
        return {token_type::lbrace, "{", loc};
    case '}':
        return {token_type::rbrace, "}", loc};
    case ':':
        return {token_type::colon, ":", loc};
    case ',':
        return {token_type::comma, ",", loc};
    case ';':
        return {token_type::semi, ";", loc};
    case '+':
        return {token_type::plus, "+", loc};
    case '*':
        return {token_type::asterik, "*", loc};
    case '%':
        return {token_type::percent, "%", loc};
    case '?':
        return {token_type::question, "?", loc};
    case '/':
        // at this point, we know that this is not a comment
        assert(peek_char() != current_char);
        return {token_type::slash, "/", loc};
    case '&':
        if (peek_char() == current_char) {
            next_char();
            return {token_type::double_and, "&&", loc};
        }
        return {token_type::amp, "&", loc};
    case '|':
        if (peek_char() == current_char) {
            next_char();
            return {token_type::double_or, "||", loc};
        }
        print_error("Single '|' is not currently a meaningful token");
        return {token_type::unknown, "|", loc};
    case '-':
        if (peek_char() == '>') {
            // found arrow
            next_char();
            return {token_type::arrow, "->", loc};
        }
        return {token_type::minus, "-", loc};
    case '<':
        if (peek_char() == '=') {
            next_char();
            return {token_type::le, "<=", loc};
        }
        return {token_type::lt, "<", loc};
    case '>':
        if (peek_char() == '=') {
            next_char();
            return {token_type::ge, ">=", loc};
        }
        return {token_type::gt, ">", loc};
    case '=':
        if (peek_char() == '=') {
            next_char();
            return {token_type::eq, "==", loc};
        }
        return {token_type::equal, "=", loc};
    case '\"': {
        std::string to_ret;
        to_ret += current_char;
        while (peek_char() != current_char and peek_char() != EOF) {
            if (peek_char() == '\\') {
                next_char();
                to_ret += next_escaped();
            } else {
                to_ret += next_char();
            }
        }
        // consume the quote
        to_ret += next_char();
        return {token_type::string, std::move(to_ret), loc};
    }
    case '\'': {
        std::string to_ret;
        to_ret += current_char;
        if (peek_char() == current_char) {
            print_error("To put `'` as a character literal, `'\\''` needs to be used");
            return {token_type::unknown, std::move(to_ret), loc};
        }

        if (peek_char() == '\\') {
            next_char();
            to_ret += next_escaped();
        } else {
            to_ret += next_char();
        }

        // consume the quote
        to_ret += next_char();
        if (to_ret.front() != to_ret.back()) {
            print_error(
                "Unclosed `\'`?; expected either only 1 character or an escape sequence; found ",
                to_ret);
            return {token_type::unknown, std::move(to_ret), loc};
        }
        return {token_type::character, std::move(to_ret), loc};
    }
    case '.':
        return {token_type::dot, ".", loc};
    default:
        print_error("Unknown character: ", static_cast<unsigned>(current_char), " \'", current_char,
                    '\'');
        return {token_type::unknown, std::string(1, current_char), loc};
    }
}

char lexer::next_char() {
    if (current_pos >= length) { return EOF; }
    // NOLINTNEXTLINE (*-pointer-arithmetic)
    if (data[current_pos] == '\n') {
        ++line_num;
        col_num = 0;
    } else {
        ++col_num;
    }
    // NOLINTNEXTLINE (*-pointer-arithmetic)
    return data[current_pos++];
}

char lexer::peek_char(unsigned offset) {
    if (offset + current_pos >= length) { return EOF; }
    // NOLINTNEXTLINE (*-pointer-arithmetic)
    return data[current_pos + offset];
}

bool lexer::next_chars(const std::string & text, unsigned offset) {
    for (auto i = 0U; i < text.size(); ++i) {
        if (peek_char(i + offset) != text[i]) { return false; }
    }
    return true;
}
