#include "new_parser.hpp"

#include "ast/base_nodes.hpp"
#include "ast/node_utils.hpp"
#include "ast/nodes.hpp"
#include "ast/nodes_forward.hpp"
#include "ast/stmt_nodes.hpp"
#include "ast/top_lvl_nodes.hpp"
#include "unistd.h" // close
#include "utils/string_utils.hpp"
#include <sys/mman.h> // mmap
#include <sys/stat.h> // fstat

#include <cassert>
#include <cctype>   // isspace
#include <fcntl.h>  // open
#include <iostream> // cerr
#include <map>
#include <memory>

std::unique_ptr<parser> parser::from_file(const std::string & filename) {

    auto fd = open(filename.c_str(), O_CLOEXEC | O_RDONLY);
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

    if (tok == token_type::eof) {
        // TODO: spdlog
        error = "Found empty file";
        return nullptr;
    }

    if (tok == token_type::from) { to_ret->imports = parse_imports(); }

    while (peek_token() != token_type::eof) {
        if (peek_token() == token_type::export_) {
            to_ret->append(parse_exports());
        } else {
            auto item = parse_top_level();
            if (item == nullptr) {
                std::cerr << "Error: " << error << std::endl;
                assert(false);
            }
            to_ret->append(std::move(item));
        }
    }
    return to_ret;
}

ast::top_lvl_ptr parser::parse_top_level() {

    switch (peek_token().type) {
    case token_type::identifier:
        // parse function
        return parse_function();
    case token_type::const_:
        // parse function
        return parse_const_decl();
    default:
        error = "Unexpected " + next_token().text;
        return nullptr;
    }
}

std::vector<ast::top_lvl_ptr> parser::parse_exports() {
    assert(next_token() == token_type::export_);

    std::vector<ast::top_lvl_ptr> items;
    if (consume_if(token_type::lbrace).has_value()) {
        while (peek_token() != token_type::rbrace) { items.push_back(parse_top_level()); }
        assert(next_token() == token_type::rbrace);
    } else {
        items.push_back(parse_top_level());
    }

    for (auto & item : items) { item->should_export(true); }
    return items;
}

std::map<std::string, std::vector<std::string>> parser::parse_imports() {

    std::map<std::string, std::vector<std::string>> to_ret;

    // parse possible imports
    while (peek_token() == token_type::from) {
        assert(next_token() == token_type::from);
        auto filename = next_token();
        assert(filename == token_type::string);

        assert(next_token() == token_type::import_);

        bool more_ids = peek_token() == token_type::identifier;
        std::vector<std::string> identifiers;
        while (more_ids) {
            identifiers.push_back(next_token().text);

            if (consume_if(token_type::comma).has_value()) {
                assert(peek_token() == token_type::identifier);
                continue;
            }

            if (consume_if(token_type::semi).has_value()) { break; }

            switch (peek_token().type) {
            case token_type::identifier:
            case token_type::from:
                // end of this import
                more_ids = false;
                break;
            default:
                std::cerr << "Unexpected " << peek_token().text << " in import." << std::endl;
                assert(false);
            }
        }

        to_ret.emplace(unquote(filename.text), std::move(identifiers));
    }
    return to_ret;
}

std::unique_ptr<ast::func_decl> parser::parse_function() {

    auto tok = next_token();
    auto func_name = tok.text;
    assert(tok == token_type::identifier);

    tok = next_token();
    assert(tok == token_type::lparen);

    std::vector<ast::typed_identifier> args;
    while (peek_token() == token_type::identifier or peek_token() == token_type::prim_type) {
        auto first_id = next_token();

        // type id or id : type
        // if the first token we see is a primitive type, there should not be a colon.
        auto name_first
            = first_id != token_type::prim_type and consume_if(token_type::colon).has_value();

        auto second_id = next_token();
        assert(second_id == token_type::identifier or second_id == token_type::prim_type);

        auto arg = name_first
                     ? ast::typed_identifier{std::move(first_id.text), std::move(second_id.text)}
                     : ast::typed_identifier{std::move(second_id.text), std::move(first_id.text)};
        args.push_back(std::move(arg));

        if (consume_if(token_type::comma).has_value()) { continue; }

        switch (peek_token().type) {
        case token_type::rparen:
            // this will be consumed after the loop.
            // just ignore
            break;
        default:
            // this should not happen
            assert(false);
        }
    }

    tok = next_token();
    assert(tok == token_type::rparen);

    ast::func_decl::header func_header{std::move(func_name), std::move(args)};
    if (consume_if(token_type::arrow).has_value()) {
        func_header.set_ret_type(parse_type());
    } else {
        func_header.set_ret_type("unit");
    }

    // check for expression body
    if (consume_if(token_type::equal).has_value()) {
        auto body = parse_expression();
        return std::make_unique<ast::func_decl>(
            std::move(func_header), std::make_unique<ast::return_stmt>(std::move(body)));
    }

    auto body = parse_statement();
    if (body == nullptr) {
        error = "Could not find body for " + func_header.name();
        return nullptr;
    }

    return std::make_unique<ast::func_decl>(std::move(func_header), std::move(body));
}

std::unique_ptr<ast::const_decl> parser::parse_const_decl() {
    assert(next_token() == token_type::const_);

    assert(peek_token() == token_type::identifier);
    auto id = next_token().text;
    assert(next_token() == token_type::colon);
    auto type = parse_type();

    assert(next_token() == token_type::equal);
    auto value = parse_expression();
    assert(value != nullptr);

    // optional consume ';'
    consume_if(token_type::semi);
    return std::make_unique<ast::const_decl>(ast::typed_identifier{std::move(id), std::move(type)},
                                             std::move(value));
}

ast::stmt_ptr parser::parse_statement() {
    switch (peek_token().type) {
    case token_type::lbrace:
        return parse_compound_statement();
    case token_type::return_:
        return parse_return_statement();
    case token_type::if_:
        return parse_if_statement();
    case token_type::let:
        return parse_let_statement();
    case token_type::identifier: {
        auto func_call = std::make_unique<ast::func_call_stmt>(parse_func_call());
        // optionally consume a semi
        consume_if(token_type::semi);
        return func_call;
    }
    default:
        error = "Unexpected " + peek_token().text + " at start of statement";
        return nullptr;
    }
}

ast::stmt_ptr parser::parse_compound_statement() {
    auto tok = next_token();
    assert(tok == token_type::lbrace);

    auto to_ret = std::make_unique<ast::stmt_sequence>();

    while (peek_token() != token_type::rbrace) {
        auto stmt = parse_statement();
        if (stmt == nullptr) { return nullptr; }
        to_ret->append(std::move(stmt));
    }

    assert(next_token() == token_type::rbrace);

    return to_ret;
}

std::unique_ptr<ast::if_stmt> parser::parse_if_statement() {
    assert(next_token() == token_type::if_);
    auto condition = parse_expression();

    const bool can_have_else = peek_token() == token_type::lbrace;
    auto then_block = parse_statement();

    ast::stmt_ptr else_block;
    if (can_have_else and consume_if(token_type::else_).has_value()) {
        else_block = parse_statement();
    }
    return std::make_unique<ast::if_stmt>(std::move(condition), std::move(then_block),
                                          std::move(else_block));
}

std::unique_ptr<ast::return_stmt> parser::parse_return_statement() {
    assert(next_token() == token_type::return_);

    if (consume_if(token_type::semi).has_value()) {
        // no expression
        return std::make_unique<ast::return_stmt>();
    }

    // expression
    auto value = parse_expression();
    assert(next_token() == token_type::semi);
    return std::make_unique<ast::return_stmt>(std::move(value));
}

std::unique_ptr<ast::let_stmt> parser::parse_let_statement() {
    assert(next_token() == token_type::let);

    assert(peek_token() == token_type::identifier);
    auto id = next_token().text;
    std::string type = "auto";
    if (consume_if(token_type::colon).has_value()) {
        assert(peek_token() == token_type::identifier);
        type = next_token().text;
    }

    assert(next_token() == token_type::equal);

    auto val = parse_expression();
    assert(val != nullptr);
    assert(next_token() == token_type::semi);
    return std::make_unique<ast::let_stmt>(ast::typed_identifier{std::move(id), std::move(type)},
                                           std::move(val));
}

std::string parser::parse_type() {
    switch (peek_token().type) {
    case token_type::identifier:
    case token_type::prim_type:
        return next_token().text;
    default:
        error = "Expected a type. Found " + peek_token().text;
        assert(false);
    }
}

ast::expr_ptr parser::parse_expression() { return parse_boolean_expression(); }
ast::expr_ptr parser::parse_boolean_expression() {
    auto expr = parse_comparison();
    if (peek_token() == token_type::double_and or peek_token() == token_type::double_or) {
        auto tok = next_token();
        assert(tok == token_type::double_and or tok == token_type::double_or);
        using operand = ast::binary_expr::operand;
        auto rhs = parse_comparison();
        assert(rhs != nullptr);
        expr = std::make_unique<ast::binary_expr>(
            std::move(expr), tok == token_type::double_or ? operand::bool_or : operand::bool_and,
            std::move(rhs));
    }
    return expr;
}

ast::expr_ptr parser::parse_comparison() {
    auto expr = parse_additive();
    if (auto tok_type = peek_token(); tok_type == token_type::lt or tok_type == token_type::le
                                      or tok_type == token_type::gt or tok_type == token_type::ge
                                      or tok_type == token_type::eq or tok_type == token_type::ne) {
        auto tok = next_token();
        using operand = ast::binary_expr::operand;
        auto rhs = parse_additive();
        switch (tok.type) {
        case token_type::le:
            return std::make_unique<ast::binary_expr>(std::move(expr), operand::le, std::move(rhs));
        case token_type::lt:
            return std::make_unique<ast::binary_expr>(std::move(expr), operand::lt, std::move(rhs));
        case token_type::ge:
            return std::make_unique<ast::binary_expr>(std::move(expr), operand::ge, std::move(rhs));
        case token_type::gt:
            return std::make_unique<ast::binary_expr>(std::move(expr), operand::gt, std::move(rhs));
        case token_type::eq:
            return std::make_unique<ast::binary_expr>(std::move(expr), operand::eq, std::move(rhs));
        case token_type::ne:
            return std::make_unique<ast::binary_expr>(std::move(expr), operand::ne, std::move(rhs));
        default:
            assert(false);
        }
    }
    return expr;
}

ast::expr_ptr parser::parse_additive() {
    auto expr = parse_multiplicative();
    if (auto tok_type = peek_token();
        tok_type == token_type::plus or tok_type == token_type::minus) {
        auto tok = next_token();
        using operand = ast::binary_expr::operand;
        auto rhs = parse_multiplicative();
        switch (tok.type) {
        case token_type::plus:
            return std::make_unique<ast::binary_expr>(std::move(expr), operand::add,
                                                      std::move(rhs));
        case token_type::minus:
            return std::make_unique<ast::binary_expr>(std::move(expr), operand::sub,
                                                      std::move(rhs));
        default:
            assert(false);
        }
    }
    return expr;
}

ast::expr_ptr parser::parse_multiplicative() {
    auto expr = parse_unary();
    if (auto tok_type = peek_token(); tok_type == token_type::percent
                                      or tok_type == token_type::asterik
                                      or tok_type == token_type::slash) {
        auto tok = next_token();
        using operand = ast::binary_expr::operand;
        auto rhs = parse_unary();
        switch (tok.type) {
        case token_type::percent:
            return std::make_unique<ast::binary_expr>(std::move(expr), operand::mod,
                                                      std::move(rhs));
        case token_type::asterik:
            return std::make_unique<ast::binary_expr>(std::move(expr), operand::mult,
                                                      std::move(rhs));
        case token_type::slash:
            return std::make_unique<ast::binary_expr>(std::move(expr), operand::div,
                                                      std::move(rhs));
        default:
            assert(false);
        }
    }
    return expr;
}

ast::expr_ptr parser::parse_unary() {

    using operand = ast::unary_expr::operand;
    switch (peek_token().type) {
    case token_type::minus: {
        // - expr
        next_token();
        auto expr = parse_atom();
        assert(expr != nullptr);
        return std::make_unique<ast::unary_expr>(operand::negate, std::move(expr));
    }
    case token_type::exclam: {
        // ! expr
        next_token();
        auto expr = parse_atom();
        assert(expr != nullptr);
        return std::make_unique<ast::unary_expr>(operand::bool_not, std::move(expr));
    }
    default:
        return parse_atom();
    }
}

ast::expr_ptr parser::parse_atom() {
    // parens
    auto tok = peek_token();
    if (tok == token_type::lparen) {
        next_token();
        auto expr = parse_expression();
        assert(next_token() == token_type::rparen);
        return expr;
    }

    using val_type = ast::user_val::value_type;
    if (tok == token_type::integer or tok == token_type::floating or tok == token_type::string
        or tok == token_type::boolean or tok == token_type::character) {
        switch (tok.type) {
        case token_type::string:
            return std::make_unique<ast::user_val>(next_token().text, val_type::string);
        case token_type::character:
            return std::make_unique<ast::user_val>(next_token().text, val_type::character);
        case token_type::integer:
            return std::make_unique<ast::user_val>(next_token().text, val_type::integer);
        case token_type::floating:
            return std::make_unique<ast::user_val>(next_token().text, val_type::floating);
        case token_type::boolean:
            return std::make_unique<ast::user_val>(next_token().text, val_type::boolean);
        default:
            assert(false);
        }
    }

    assert(tok == token_type::identifier);
    auto id = next_token().text;
    if (peek_token() == token_type::lparen) {
        return std::make_unique<ast::func_call_expr>(parse_func_call(std::move(id)));
    }

    return std::make_unique<ast::user_val>(std::move(id), val_type::identifier);
}

ast::func_call_data parser::parse_func_call(std::optional<std::string> func_name) {
    auto name = [&] {
        if (func_name.has_value()) {
            // we have already eaten the id
            assert(next_token() == token_type::lparen);
            return func_name.value();
        }

        // we need to eat the id
        assert(peek_token() == token_type::identifier);
        auto name = next_token().text;
        assert(next_token() == token_type::lparen);
        return name;
    }();

    // we have eaten the lparen here
    std::vector<ast::expr_ptr> args;
    while (peek_token() != token_type::rparen) {
        auto expr = parse_expression();
        assert(expr != nullptr);
        args.push_back(std::move(expr));
        switch (peek_token().type) {
        case token_type::rparen:
            break;
        case token_type::comma:
            next_token();
            break;
        default:
            assert(false);
        }
    }

    assert(next_token() == token_type::rparen);

    return {std::move(name), std::move(args)};
}

parser::token parser::next_token() {

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

parser::token parser::next_identifier() {

    std::string to_ret;

    // parse a word
    // XXX: this may be bad on performance
    while (isalnum(peek_char()) != 0 or peek_char() == '_') { to_ret += next_char(); }

    static const std::map<std::string, parser::token_type> reserved_words{
        // alternate tokens
        {"and", token_type::double_and},
        {"equals", token_type::eq},
        {"is", token_type::colon},
        {"or", token_type::double_or},
        // keywords
        {"const", token_type::const_},
        {"else", token_type::else_},
        {"export", token_type::export_},
        {"from", token_type::from},
        {"if", token_type::if_},
        {"import", token_type::import_},
        {"let", token_type::let},
        {"ret", token_type::return_},
        {"return", token_type::return_},
        // primitive types
        {"bool", token_type::prim_type},
        {"char", token_type::prim_type},
        {"float", token_type::prim_type},
        {"int", token_type::prim_type},
        {"string", token_type::prim_type},
        {"unit", token_type::prim_type},
    };

    if (auto iter = reserved_words.find(to_ret); iter != reserved_words.end()) {
        return {iter->second, std::move(to_ret)};
    }
    return {token_type::identifier, std::move(to_ret)};
}

parser::token parser::next_number() {

    auto c = next_char();
    std::string to_ret;
    to_ret += c;
    if (c == '0') {
        // either we are hexadecimal or just 0
        if (tolower(peek_char()) == 'x') {
            // remove the 'x' or 'X'
            to_ret += next_char();
            // consume all hexadecimal digits
            while (isxdigit(peek_char()) != 0) { to_ret += next_char(); }
        }

        return {token_type::integer, std::move(to_ret)};
    }

    assert(isdigit(c));
    while (isdigit(peek_char()) != 0) { to_ret += next_char(); }

    if (peek_char() != '.') { return {token_type::integer, to_ret}; }

    assert(false);
}

parser::token parser::next_symbol() {

    switch (const auto c = next_char(); c) {
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
    case ';':
        return {token_type::semi, ";"};
    case '+':
        return {token_type::plus, "+"};
    case '*':
        return {token_type::asterik, "*"};
    case '%':
        return {token_type::percent, "%"};
    case '/':
        // at this point, we know that this is not a comment
        assert(peek_char() != c);
        return {token_type::slash, "/"};
    case '&':
        if (peek_char() == c) {
            next_char();
            return {token_type::double_and, "&&"};
        }
        assert(false);
    case '|':
        if (peek_char() == c) {
            next_char();
            return {token_type::double_or, "||"};
        }
        assert(false);
    case '-':
        if (peek_char() == '>') {
            // found arrow
            next_char();
            return {token_type::arrow, "->"};
        }
        return {token_type::minus, "-"};
    case '<':
        if (peek_char() == '=') {
            next_char();
            return {token_type::le, "<="};
        }
        return {token_type::lt, "<"};
        break;
    case '>':
        if (peek_char() == '=') {
            next_char();
            return {token_type::ge, ">="};
        }
        return {token_type::gt, ">"};
    case '=':
        if (peek_char() == '=') {
            next_char();
            return {token_type::eq, "=="};
        }
        return {token_type::equal, "="};
    case '\"': {
        std::string to_ret;
        to_ret += c;
        while (peek_char() != c) { to_ret += next_char(); }
        // consume the quote
        to_ret += next_char();
        return {token_type::string, std::move(to_ret)};
    } break;
    case '\'': {
        std::string to_ret;
        to_ret += c;
        assert(peek_char() != c);

        if (peek_char() == '\\') { to_ret += next_char(); }
        to_ret += next_char();

        // consume the quote
        to_ret += next_char();
        return {token_type::character, std::move(to_ret)};
    } break;
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
