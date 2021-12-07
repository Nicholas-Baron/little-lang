#include "new_parser.hpp"

#include "ast/nodes.hpp"
#include "ast/top_lvl_nodes.hpp"
#include "unistd.h"   // close
#include <sys/mman.h> // mmap
#include <sys/stat.h> // fstat

#include <cassert>
#include <cctype>   // isspace
#include <fcntl.h>  // open
#include <iostream> // cerr
#include <map>

std::unique_ptr<parser> parser::from_file(const std::string & filename) {

    auto fd = open(filename.c_str(), O_CLOEXEC | O_WRONLY);
    if (fd == -1) {
        perror("parser open");
        return nullptr;
    }

    struct stat file_stats;
    if (fstat(fd, &file_stats) == -1) {
        perror("parser stat");
        return nullptr;
    }

    const auto file_length = file_stats.st_size;

    auto * data = (char *)mmap(nullptr, file_length, PROT_READ, MAP_PRIVATE, fd, 0);
    if (data == MAP_FAILED) {
        perror("parser mmap");
        return nullptr;
    }

    if (close(fd) != 0) {
        perror("parser close");
        return nullptr;
    }

    return std::unique_ptr<parser>(new parser(filename, data, file_length));
}

std::unique_ptr<parser> parser::from_buffer(std::string & buffer) {
    return std::unique_ptr<parser>(new parser(buffer.c_str(), buffer.size()));
}

parser::parser(std::string filename, const char * data, size_t size)
    : filename{std::move(filename)}
    , data{data}
    , length{size}
    , type{data_type::mmapped} {}

parser::parser(const char * data, size_t size)
    : filename{"internal buffer"}
    , data{data}
    , length{size}
    , type{data_type::read_buffer} {}

parser::~parser() {
    if (type == data_type::mmapped) {
        if (munmap(const_cast<char *>(data), length) == -1) { perror("parser munmap"); }
    }
}

std::unique_ptr<ast::top_level_sequence> parser::parse() {

    auto to_ret = std::make_unique<ast::top_level_sequence>();

    auto tok = peek_token();

    if (tok.first == token_type::eof) {
        error = "Found empty file";
        return nullptr;
    }

    // parse possible imports
    if (tok.first == token_type::from) { assert(false); }

    while (tok.first != token_type::eof) {
        // parse top level items
        switch (tok.first) {
        case token_type::identifier:
            // parse function
            to_ret->append(parse_function());
            break;
        default:
            if (tok.first == token_type::eof) {
                error = "Unexpected end of file";
            } else {
                error = "Unexpected " + tok.second;
            }
            return nullptr;
        }
        tok = peek_token();
    }
    return to_ret;
}

std::unique_ptr<ast::func_decl> parser::parse_function() {

    auto tok = next_token();
    auto func_name = tok.second;
    assert(tok.first == token_type::identifier);

    tok = next_token();
    assert(tok.first == token_type::lparen);

    tok = next_token();
    assert(tok.first == token_type::rparen);

    ast::func_decl::header func_header{std::move(func_name), {}};
    if (peek_token().first == token_type::arrow) {
        assert(next_token().first == token_type::arrow);

        auto ret_tok = next_token();
        assert(ret_tok.first == token_type::identifier);
        func_header.set_ret_type(std::move(ret_tok.second));
    }

    auto body = parse_statement();
    if (body == nullptr) { return nullptr; }

    return std::make_unique<ast::func_decl>(std::move(func_header), std::move(body));
}

ast::stmt_ptr parser::parse_statement() {
    switch (auto tok = peek_token(); tok.first) {
    case token_type::lbrace:
        return parse_compound_statement();
    default:
        error = "Unexpected " + tok.second + " at start of statement";
        return nullptr;
    }
}

ast::stmt_ptr parser::parse_compound_statement() {
    auto tok = next_token();
    assert(tok.first == token_type::lbrace);

    auto to_ret = std::make_unique<ast::stmt_sequence>();

    tok = peek_token();
    while (tok.first != token_type::rbrace) {
        auto stmt = parse_statement();
        if (stmt == nullptr) { return nullptr; }
        to_ret->append(std::move(stmt));
        tok = peek_token();
    }

    assert(next_token().first == token_type::rbrace);

    return to_ret;
}

std::pair<parser::token_type, std::string> parser::next_token() {

    if (peeked_token.has_value()) {
        auto result = peeked_token.value();
        peeked_token.reset();
        return result;
    }

    while (true) {
        // go to first non-whitespace character
        while (isspace(peek_char()) != 0) { next_char(); }

        // TODO: Support "[Cc]omment" to start a comment
        auto found_comment = (peek_char() == '/' and peek_char(1) == '/') or peek_char() == '#';
        if (not found_comment) { break; }

        // Skip the comment line
        while (peek_char() != '\n' and peek_char() != '\r') { next_char(); }
    }

    if (peek_char() == EOF) { return {token_type::eof, ""}; }

    // we are now at the first meaningful token
    if (isalpha(peek_char()) != 0) { return next_identifier(); }
    if (isdigit(peek_char()) != 0) { return next_number(); }
    return next_symbol();
}

std::pair<parser::token_type, std::string> parser::next_identifier() {

    std::string to_ret;

    // parse a word
    // XXX: this may be bad on performance
    while (isalnum(peek_char()) != 0) { to_ret += next_char(); }

    static const std::map<std::string, parser::token_type> reserved_words{
        {"is", token_type::colon}};

    if (auto iter = reserved_words.find(to_ret); iter != reserved_words.end()) {
        return {iter->second, std::move(to_ret)};
    }
    return {token_type::identifier, std::move(to_ret)};
}

std::pair<parser::token_type, std::string> parser::next_number() {

    auto c = next_char();
    std::string to_ret;
    to_ret += c;
    if (c == '0') {
        // either we are hexadecimal or just 0
        if (peek_char() != 'x') { return {token_type::integer, std::move(to_ret)}; }

        // we are hexadecimal
        while (isxdigit(peek_char()) != 0) { to_ret += next_char(); }
        return {token_type::integer, std::move(to_ret)};
    }
    assert(false);
}

std::pair<parser::token_type, std::string> parser::next_symbol() {

    switch (auto c = next_char(); c) {
    case EOF:
        return {token_type::eof, ""};
    case '(':
        return {token_type::lparen, "("};
    case ')':
        return {token_type::rparen, ")"};
    case '{':
        return {token_type::lbrace, "{"};
    case '}':
        return {token_type::rbrace, "}"};
    case ':':
        return {token_type::colon, ":"};
    case ',':
        return {token_type::comma, ","};
    case '-':
        if (peek_char() == '>') {
            // found arrow
            std::string to_ret;
            (to_ret += c) += next_char();
            return {token_type::arrow, to_ret};
        }
        assert(false);
        break;
    default:
        std::cerr << "Unknown character: " << static_cast<unsigned>(c) << " \'" << c << '\''
                  << std::endl;
        assert(false);
    }
}

char parser::next_char() {
    if (current_pos >= length) { return EOF; }
    return data[current_pos++];
}

char parser::peek_char(unsigned offset) {
    if (offset + current_pos >= length) { return EOF; }
    return data[current_pos + offset];
}
