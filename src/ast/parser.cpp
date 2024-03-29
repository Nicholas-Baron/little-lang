#include "parser.hpp"

#include "node_utils.hpp"
#include "nodes.hpp"
#include "operations.hpp"
#include "type.hpp"

#include <algorithm> // find_if
#include <cassert>
#include <cstring> // strncmp
#include <filesystem>
#include <iostream> // cerr
#include <map>
#include <memory> // unique_ptr

#include <string_utils.hpp> // unquote

std::unique_ptr<parser> parser::from_file(const std::string & filename,
                                          const std::filesystem::path & project_root,
                                          ast::type_context & ty_context) {
    auto lexer = lexer::from_file(filename, project_root);
    if (lexer == nullptr) { return nullptr; }
    // NOTE: `make_unique` does not like private constructors.
    return std::unique_ptr<parser>(new parser{std::move(lexer), ty_context});
}

std::unique_ptr<parser> parser::from_buffer(std::string & buffer, ast::type_context & ty_context) {
    return parser::from_buffer(buffer.c_str(), buffer.size(), ty_context);
}

std::unique_ptr<parser> parser::from_buffer(const char * data, size_t size,
                                            ast::type_context & ty_context) {
    auto lexer = lexer::from_buffer(data, size);
    if (lexer == nullptr) { return nullptr; }
    // NOTE: `make_unique` does not like private constructors.
    return std::unique_ptr<parser>(new parser{std::move(lexer), ty_context});
}

std::unique_ptr<ast::top_level_sequence> parser::parse() {

    auto to_ret = std::make_unique<ast::top_level_sequence>(lex->peek_token().location);

    to_ret->filename = lex->file_name();

    auto tok = lex->peek_token();

    if (tok == lexer::token_type::eof) {
        // We do not allow an empty module.
        print_error(Location{}, "Found empty file");
        return nullptr;
    }

    // There may be some imports to parse.
    if (tok == lexer::token_type::from) { to_ret->imports = parse_imports(); }

    while (lex->has_more_tokens()) {
        // The only special case here is `export`, as we do not allow `export export`.
        if (lex->peek_token() == lexer::token_type::export_) {
            to_ret->append(parse_exports());
        } else {
            to_ret->append(parse_top_level());
        }
    }

    if (not error_printout.empty()) { return nullptr; }

    return to_ret;
}

ast::top_lvl_ptr parser::parse_top_level() {

    switch (lex->peek_token().type) {
    case lexer::token_type::identifier:
        // Parse a function or struct
        if (lex->peek_token(1) == lexer::token_type::lparen) {
            return parse_function();
        } else if (lex->peek_token(1) == lexer::token_type::lbrace) {
            return parse_struct_decl();
        }
        print_error(lex->peek_token(1).location, "Unexpected ", lex->peek_token(1).text, " after ",
                    lex->peek_token().text);
        lex->next_token();
        return nullptr;
    case lexer::token_type::const_:
        // Parse a constant
        return parse_const_decl();
    default:
        print_error(lex->peek_token().location, "Unexpected ", lex->next_token().text,
                    " for top level item");
        return nullptr;
    }
}

std::vector<ast::top_lvl_ptr> parser::parse_exports() {
    assert(lex->next_token() == lexer::token_type::export_);

    std::vector<ast::top_lvl_ptr> items;
    if (lex->consume_if(lexer::token_type::lbrace).has_value()) {
        // We have found an export block.
        // All items inside of it need to be exported.
        while (lex->peek_token() != lexer::token_type::rbrace and lex->has_more_tokens()) {
            items.push_back(parse_top_level());
        }

        expect_token(lexer::token_type::rbrace, "}");

    } else {
        // There is only a single item to export.
        items.push_back(parse_top_level());
    }

    // Mark all parsed items as exported.
    for (auto & item : items) {
        if (item != nullptr) { item->should_export(true); }
    }
    return items;
}

std::map<std::string, std::vector<std::string>> parser::parse_imports() {

    std::map<std::string, std::vector<std::string>> to_ret;

    // Parse imports
    // Imports take the form of `from "filename" import x, y`, optionally ending in a semicolon.
    while (lex->peek_token() == lexer::token_type::from) {
        assert(lex->next_token() == lexer::token_type::from);
        auto filename = lex->next_token();
        if (filename != lexer::token_type::string) {
            print_error(filename.location, "Expected a string for filename; found ", filename.text);
        }

        expect_token(lexer::token_type::import_, "import");

        bool more_ids = lex->peek_token() == lexer::token_type::identifier;
        std::vector<std::string> identifiers;
        while (more_ids) {
            identifiers.push_back(lex->next_token().text);

            if (lex->consume_if(lexer::token_type::comma).has_value()) {
                // There are more identifiers to import from this module
                if (lex->peek_token() != lexer::token_type::identifier) {
                    auto tok = lex->next_token();
                    print_error(tok.location, "Expected an identifier; found ", tok.text);
                }
                continue;
            }

            // There are no more identifiers to import from this module
            if (lex->consume_if(lexer::token_type::semi).has_value()) { break; }

            switch (lex->peek_token().type) {
            case lexer::token_type::identifier:
                // Double identifiers signal the start of a function.
            case lexer::token_type::const_:
                // `const` signals the start of a constant.
            case lexer::token_type::from:
                // `from` signals a new import.
                more_ids = false;
                break;
            default: {
                auto tok = lex->next_token();
                print_error(
                    tok.location,
                    "Expected an identifier, `const`, or `from` after import statement; found ",
                    tok.text);
                more_ids = false;
            } break;
            }
        }

        to_ret.emplace(unquote(filename.text), std::move(identifiers));
    }
    return to_ret;
}

std::unique_ptr<ast::func_decl> parser::parse_function() {

    // Function declarations take the form of `name`,
    // followed by parenthesis-enclosed arguments,
    // followed by a return type,
    // followed by a body.

    // Parse the function's name.
    auto func_name_tok = lex->next_token();
    assert(func_name_tok == lexer::token_type::identifier);

    auto tok = lex->next_token();
    assert(tok == lexer::token_type::lparen);

    // Parse the arguments (there may be none).
    std::vector<ast::typed_identifier> args;
    while (lex->peek_token() == lexer::token_type::identifier
           or lex->peek_token() == lexer::token_type::prim_type) {
        args.push_back(parse_typed_identifier());

        // If there is a comma, there are more arguments.
        if (lex->consume_if(lexer::token_type::comma).has_value()) { continue; }

        switch (lex->peek_token().type) {
        case lexer::token_type::rparen:
            // This will be consumed after the loop.
            // Ignore it for now.
            break;
        default:
            // this should not happen
            print_error(lex->peek_token().location, "Expected `)` or `,`; found ",
                        lex->next_token().text);
            break;
        }
    }

    expect_token(lexer::token_type::rparen, ")");

    std::vector<ast::type_ptr> arg_types;
    arg_types.reserve(args.size());
    for (auto & arg : args) { arg_types.push_back(arg.type()); }

    const auto * return_type = ty_context.create_type<ast::prim_type>(ast::prim_type::type::unit);
    // Parse the optional return type.
    if (lex->consume_if(lexer::token_type::arrow).has_value()) { return_type = parse_type(); }

    // The body of a function may either be an `=` followed by an expression,
    // or just a statement.
    ast::stmt_ptr body;
    if (auto equal_sign = lex->peek_token(); equal_sign == lexer::token_type::equal) {
        // We need to inject the implied return for the expression,
        // as a function's body is just a statement
        auto location = lex->next_token().location;
        body = std::make_unique<ast::return_stmt>(location, parse_expression());
    } else {
        body = parse_statement();
    }

    if (body == nullptr) {
        print_error(func_name_tok.location, "Could not find body for ", func_name_tok.text);
        return nullptr;
    }

    auto * func_type
        = ty_context.create_type<ast::function_type>(return_type, std::move(arg_types));

    auto func_decl = std::make_unique<ast::func_decl>(std::move(func_name_tok.text), func_type,
                                                      std::move(body), func_name_tok.location);
    func_decl->params = std::move(args);
    return func_decl;
}

std::unique_ptr<ast::const_decl> parser::parse_const_decl() {
    auto location = lex->peek_token().location;
    assert(lex->next_token() == lexer::token_type::const_);

    // Parse the identifier and type of the constant
    auto typed_id = parse_typed_identifier();

    expect_token(lexer::token_type::equal, "=");

    // Parse the initializer of the constant
    auto value = parse_expression();

    // Parse optional semicolon
    lex->consume_if(lexer::token_type::semi);
    return std::make_unique<ast::const_decl>(std::move(typed_id), std::move(value), location);
}

std::unique_ptr<ast::struct_decl> parser::parse_struct_decl() {
    auto location = lex->peek_token().location;

    // Parse the typename of the struct
    auto name = lex->next_token();
    assert(name == lexer::token_type::identifier);

    // Parse the opening curly
    assert(lex->next_token() == lexer::token_type::lbrace);

    // Parse the fields
    std::vector<ast::typed_identifier> fields;
    while (not lex->consume_if(lexer::token_type::rbrace).has_value() and lex->has_more_tokens()) {
        auto new_field = parse_typed_identifier();

        if (auto iter = std::find_if(fields.begin(), fields.end(),
                                     [&new_field](auto & old_field) -> bool {
                                         return new_field.name() == old_field.name();
                                     });
            iter != fields.end()) {
            print_error(new_field.location(), "Found fields with same name ", new_field.name(),
                        " in struct ", name.text);
        }

        fields.emplace_back(std::move(new_field));
        switch (lex->peek_token().type) {
        case lexer::token_type::comma:
        case lexer::token_type::semi:
            lex->next_token();
            [[fallthrough]];
        case lexer::token_type::rbrace:
        case lexer::token_type::identifier:
            break;
        default: {
            auto error_token = lex->next_token();
            print_error(error_token.location,
                        "Expected one of the following: `,`, `;`, `{`, or identifier; Found ",
                        error_token.text);
        } break;
        }
    }

    auto * struct_type = [&] {
        // Insert the new struct type into the ast type registry
        std::vector<ast::struct_type::field_type> struct_fields;
        struct_fields.reserve(fields.size());
        for (auto & typed_id : fields) {
            struct_fields.emplace_back(typed_id.name(), typed_id.type());
        }
        return ty_context.create_type<ast::struct_type>(std::string{name.text}, lex->module_name(),
                                                        std::move(struct_fields));
    }();
    assert(struct_type != nullptr);

    return std::make_unique<ast::struct_decl>(std::move(name.text), struct_type, std::move(fields),
                                              location);
}

ast::stmt_ptr parser::parse_statement() {
    switch (lex->peek_token().type) {
    case lexer::token_type::lbrace:
        return parse_compound_statement();
    case lexer::token_type::return_:
        return parse_return_statement();
    case lexer::token_type::if_:
        return parse_if_statement();
    case lexer::token_type::let: {
        auto let_stmt = parse_let_statement();
        return let_stmt;
    }
    case lexer::token_type::identifier: {
        auto func_call = std::make_unique<ast::func_call_stmt>(parse_func_call());
        // Optionally consume a semicolon for function calls.
        lex->consume_if(lexer::token_type::semi);
        return func_call;
    }
    default:
        print_error(lex->peek_token().location, "Unexpected ", lex->peek_token().text,
                    " at start of statement");
        return nullptr;
    }
}

ast::stmt_ptr parser::parse_compound_statement() {
    // a compound statement is a `{`,
    // followed by some statements,
    // followed by a `}`.
    auto tok = lex->next_token();
    assert(tok == lexer::token_type::lbrace);

    auto to_ret = std::make_unique<ast::stmt_sequence>(tok.location);

    while (lex->peek_token() != lexer::token_type::rbrace) {
        auto stmt = parse_statement();
        if (stmt == nullptr) { return nullptr; }
        to_ret->append(std::move(stmt));
    }

    assert(lex->next_token() == lexer::token_type::rbrace);

    return to_ret;
}

std::unique_ptr<ast::if_stmt> parser::parse_if_statement() {
    auto location = lex->peek_token().location;
    assert(lex->next_token() == lexer::token_type::if_);
    auto condition = parse_expression();

    expect_token(lexer::token_type::then, "then");

    // an `if` can only have an `else` when it is written `if x {} else {}`,
    // that is the then block is a compund statement.
    const bool can_have_else = lex->peek_token() == lexer::token_type::lbrace;
    auto then_block = parse_statement();

    ast::stmt_ptr else_block;
    if (can_have_else and lex->consume_if(lexer::token_type::else_).has_value()) {
        else_block = parse_statement();
    }
    return std::make_unique<ast::if_stmt>(std::move(condition), std::move(then_block),
                                          std::move(else_block), location);
}

std::unique_ptr<ast::return_stmt> parser::parse_return_statement() {
    auto location = lex->peek_token().location;
    assert(lex->next_token() == lexer::token_type::return_);

    if (lex->consume_if(lexer::token_type::semi).has_value()) {
        // Found no expression
        return std::make_unique<ast::return_stmt>(location);
    }

    // Found an expression
    auto value = parse_expression();
    expect_token(lexer::token_type::semi, ";");
    return std::make_unique<ast::return_stmt>(location, std::move(value));
}

std::unique_ptr<ast::let_stmt> parser::parse_let_statement() {
    auto location = lex->peek_token().location;
    expect_token(lexer::token_type::let, "let");

    // A let statement is made of `let`,
    // followed by an optionally-typed identifier,
    // followed by `=`,
    // followed by an expression,
    // followed by `;`.

    auto typed_id = parse_opt_typed_identifier();

    expect_token(lexer::token_type::equal, "=");

    auto let_stmt
        = std::make_unique<ast::let_stmt>(std::move(typed_id), parse_expression(), location);
    expect_token(lexer::token_type::semi, ";");
    return let_stmt;
}

ast::typed_identifier parser::parse_opt_typed_identifier() {
    // a typed identifier is either:
    //     `type name`
    // or  `name : type`
    // However, here we need to allow just a name.

    // Assume that the first token we see is a type,
    // as that covers the identifier in the second case as well.
    auto location = lex->peek_token().location;

    if (lex->peek_token(1) == lexer::token_type::colon) {
        // the second case (`name : type`) has occured.
        auto name = lex->next_token();
        assert(name == lexer::token_type::identifier);
        assert(lex->next_token() == lexer::token_type::colon);

        return {std::move(name.text), parse_type(), location};
    }

    // Since type may be null, this covers the third (`name`) and first (`type name`) cases.
    // However, we need to check that there are at least 2 identifiers in a row before calling
    // parse_type. Otherwise, we may interpret the third case of just a name as a type.
    const auto * type = (lex->peek_token() == lexer::token_type::identifier
                         and lex->peek_token(1) != lexer::token_type::identifier)
                          ? nullptr
                          : parse_type();
    auto name = lex->next_token();
    if (name != lexer::token_type::identifier) {
        print_error(name.location, "Expected an identifier; found ", name.text);
    }
    return {std::move(name.text), type, location};
}

ast::typed_identifier parser::parse_typed_identifier() {
    // a typed identifier is either:
    //     `type name`
    // or  `name : type`

    auto location = lex->peek_token().location;

    switch (auto colon_or_name = lex->peek_token(1); colon_or_name.type) {
    case lexer::token_type::colon: {
        // the second case (`name : type`) has occured.
        auto identifier = lex->next_token();
        if (identifier != lexer::token_type::identifier) {
            print_error(identifier.location, "Expected an identifier; found ", identifier.text);
        }
        assert(lex->next_token() == lexer::token_type::colon);

        return {std::move(identifier.text), parse_type(), location};
    } break;
    case lexer::token_type::identifier: {
        // the first case (`type name`) has occured.
        const auto * type = parse_type();
        auto name = lex->next_token();

        if (name != lexer::token_type::identifier) {
            print_error(name.location, "Expected identifier; found ", name.text);
        }

        return {std::move(name.text), type, location};
    } break;
    default: {
        auto tok = lex->next_token();
        print_error(colon_or_name.location, "Expected either `:` or identifier; Found ",
                    colon_or_name.text);

        return {std::move(tok.text), nullptr, tok.location};
    }
    }
}

ast::type_ptr parser::make_prim_type(const lexer::token & type_name) {

    if (strncmp(type_name.text.c_str(), "int", 3) == 0) {
        if (type_name == "int") { return nullptr; }

        const auto start_num = type_name.text.substr(3);
        size_t end_num = 0;
        auto bit_count = std::stoul(start_num, &end_num);

        assert(start_num.size() == end_num);
        assert(bit_count > 0);
        assert(bit_count % 2 == 0);

        return ty_context.create_type<ast::int_type>(bit_count);
    }

    if (type_name == "float") {
        return ty_context.create_type<ast::prim_type>(ast::prim_type::type::float32);
    }
    if (type_name == "char") {
        return ty_context.create_type<ast::prim_type>(ast::prim_type::type::character);
    }
    if (type_name == "unit") {
        return ty_context.create_type<ast::prim_type>(ast::prim_type::type::unit);
    }
    if (type_name == "bool") {
        return ty_context.create_type<ast::prim_type>(ast::prim_type::type::boolean);
    }
    if (type_name == "string") {
        return ty_context.create_type<ast::prim_type>(ast::prim_type::type::str);
    }

    return nullptr;
}

ast::type_ptr parser::parse_type() {
    // a type can either be some primitive or a user-defined type.
    auto type_name_token = lex->peek_token();
    switch (type_name_token.type) {
    case lexer::token_type::identifier: {
        lex->next_token();
        const auto * type_ptr
            = ty_context.lookup_user_type(type_name_token.text, lex->module_name());
        if (type_ptr == nullptr) {
            print_error(type_name_token.location,
                        "Expected a type name; Could not find user type named ",
                        type_name_token.text);
        }
        return type_ptr;
    }
    case lexer::token_type::prim_type: {
        const auto * type = make_prim_type(lex->next_token());
        if (type == nullptr) {
            print_error(lex->peek_token().location, "Could not find primitive type named ",
                        lex->peek_token().text);
            return nullptr;
        }
        return type;
    }
    case lexer::token_type::amp:
        lex->next_token();
        return ty_context.create_type<ast::nonnullable_ptr_type>(parse_type());
    case lexer::token_type::question:
        lex->next_token();
        return ty_context.create_type<ast::nullable_ptr_type>(parse_type());
    default:
        lex->next_token();
        print_error(type_name_token.location, "Expected an identifier, `&`, or `?`; Found ",
                    type_name_token.text);
        return nullptr;
    }
}

ast::expr_ptr parser::parse_expression() { return parse_if_expression(); }

ast::expr_ptr parser::parse_if_expression() {

    // if expr then expr else expr
    auto possibly_if_tok = lex->peek_token();
    if (possibly_if_tok != lexer::token_type::if_) { return parse_boolean_expression(); }
    lex->next_token();

    // we did consume an if
    auto condition = parse_expression();

    expect_token(lexer::token_type::then, "then");

    auto then_branch = parse_expression();

    expect_token(lexer::token_type::else_, "else");

    auto else_branch = parse_expression();

    return std::make_unique<ast::if_expr>(possibly_if_tok.location, std::move(condition),
                                          std::move(then_branch), std::move(else_branch));
}

ast::expr_ptr parser::parse_boolean_expression() {
    auto expr = parse_comparison();
    if (lex->peek_token() == lexer::token_type::double_and
        or lex->peek_token() == lexer::token_type::double_or) {
        auto tok = lex->next_token();
        assert(tok == lexer::token_type::double_and or tok == lexer::token_type::double_or);

        auto rhs = parse_comparison();

        using operand = operation::binary;
        expr = std::make_unique<ast::binary_expr>(
            std::move(expr),
            tok == lexer::token_type::double_or ? operand::bool_or : operand::bool_and,
            std::move(rhs), tok.location);
    }
    return expr;
}

ast::expr_ptr parser::parse_comparison() {
    auto expr = parse_additive();
    if (auto tok_type = lex->peek_token();
        tok_type == lexer::token_type::lt or tok_type == lexer::token_type::le
        or tok_type == lexer::token_type::gt or tok_type == lexer::token_type::ge
        or tok_type == lexer::token_type::eq or tok_type == lexer::token_type::ne) {
        auto tok = lex->next_token();
        using operand = operation::binary;
        auto rhs = parse_additive();
        switch (tok.type) {
        case lexer::token_type::le:
            expr = std::make_unique<ast::binary_expr>(std::move(expr), operand::le, std::move(rhs),
                                                      tok.location);
            break;
        case lexer::token_type::lt:
            expr = std::make_unique<ast::binary_expr>(std::move(expr), operand::lt, std::move(rhs),
                                                      tok.location);
            break;
        case lexer::token_type::ge:
            expr = std::make_unique<ast::binary_expr>(std::move(expr), operand::ge, std::move(rhs),
                                                      tok.location);
            break;
        case lexer::token_type::gt:
            expr = std::make_unique<ast::binary_expr>(std::move(expr), operand::gt, std::move(rhs),
                                                      tok.location);
            break;
        case lexer::token_type::eq:
            expr = std::make_unique<ast::binary_expr>(std::move(expr), operand::eq, std::move(rhs),
                                                      tok.location);
            break;
        case lexer::token_type::ne:
            expr = std::make_unique<ast::binary_expr>(std::move(expr), operand::ne, std::move(rhs),
                                                      tok.location);
            break;
        default:
            assert(false);
        }
    }
    return expr;
}

ast::expr_ptr parser::parse_additive() {
    auto expr = parse_multiplicative();
    if (auto tok_type = lex->peek_token();
        tok_type == lexer::token_type::plus or tok_type == lexer::token_type::minus) {
        auto tok = lex->next_token();
        auto rhs = parse_multiplicative();
        using operand = operation::binary;
        switch (tok.type) {
        case lexer::token_type::plus:
            expr = std::make_unique<ast::binary_expr>(std::move(expr), operand::add, std::move(rhs),
                                                      tok.location);
            break;
        case lexer::token_type::minus:
            expr = std::make_unique<ast::binary_expr>(std::move(expr), operand::sub, std::move(rhs),
                                                      tok.location);
            break;
        default:
            assert(false);
        }
    }
    return expr;
}

ast::expr_ptr parser::parse_multiplicative() {
    auto expr = parse_cast();
    if (auto tok_type = lex->peek_token(); tok_type == lexer::token_type::percent
                                           or tok_type == lexer::token_type::asterik
                                           or tok_type == lexer::token_type::slash) {
        auto tok = lex->next_token();
        using operand = operation::binary;
        auto rhs = parse_cast();
        switch (tok.type) {
        case lexer::token_type::percent:
            expr = std::make_unique<ast::binary_expr>(std::move(expr), operand::mod, std::move(rhs),
                                                      tok.location);
            break;
        case lexer::token_type::asterik:
            expr = std::make_unique<ast::binary_expr>(std::move(expr), operand::mult,
                                                      std::move(rhs), tok.location);
            break;
        case lexer::token_type::slash:
            expr = std::make_unique<ast::binary_expr>(std::move(expr), operand::div, std::move(rhs),
                                                      tok.location);
            break;
        default:
            assert(false);
        }
    }
    return expr;
}

ast::expr_ptr parser::parse_cast() {
    auto expr = parse_unary();
    while (lex->peek_token() == lexer::token_type::as) {
        auto location = lex->next_token().location;
        const auto * type = parse_type();
        expr = std::make_unique<ast::cast_expr>(std::move(expr), type, location);
    }
    return expr;
}

ast::expr_ptr parser::parse_unary() {

    using operand = operation::unary;

    std::optional<std::pair<Location, operand>> unary_op;

    switch (lex->peek_token().type) {
    case lexer::token_type::minus: {
        // - expr
        auto location = lex->next_token().location;
        unary_op = std::make_pair(location, operand::negate);
    } break;
    case lexer::token_type::exclam: {
        // ! expr
        auto location = lex->next_token().location;
        unary_op = std::make_pair(location, operand::bool_not);
    } break;
    case lexer::token_type::asterik: {
        // * expr
        auto location = lex->next_token().location;
        unary_op = std::make_pair(location, operand::deref);
    } break;
    case lexer::token_type::amp: {
        // & expr
        auto location = lex->next_token().location;
        unary_op = std::make_pair(location, operand::addrof);
    } break;
    default:
        return parse_member_access();
    }

    assert(unary_op.has_value());

    auto [location, operation] = *unary_op;
    auto expr = parse_atom();
    return std::make_unique<ast::unary_expr>(operation, std::move(expr), location);
}

ast::expr_ptr parser::parse_member_access() {

    // Consider the case of `(x.y).z`.
    // We must try to parse an atom for the parenthesis-enclosed lhs.
    auto expr = parse_atom();

    auto possibly_dot = lex->peek_token();

    while (possibly_dot == lexer::token_type::dot) {
        // the rhs could be:
        //  - an identifier
        //  - an integer (if tuples get implemented)

        auto dot = lex->next_token();
        auto tok = lex->next_token();

        if (tok != lexer::token_type::identifier) {
            print_error(tok.location, "Expected an identifier for member access; found ", tok.text);
            break;
        }

        auto rhs = std::make_unique<ast::user_val>(std::move(tok.text), literal_type::identifier,
                                                   tok.location);
        expr = std::make_unique<ast::binary_expr>(std::move(expr), operation::binary::member_access,
                                                  std::move(rhs), dot.location);
        possibly_dot = lex->peek_token();
    }

    return expr;
}

ast::expr_ptr parser::parse_atom() {
    // parens
    auto tok = lex->peek_token();
    if (tok == lexer::token_type::lparen) {
        lex->next_token();
        auto expr = parse_expression();
        expect_token(lexer::token_type::rparen, ")");
        return expr;
    }

    // literals
    using val_type = literal_type;
    if (tok == lexer::token_type::integer or tok == lexer::token_type::floating
        or tok == lexer::token_type::string or tok == lexer::token_type::boolean
        or tok == lexer::token_type::character or tok == lexer::token_type::null) {
        switch (tok.type) {
        case lexer::token_type::string:
            return std::make_unique<ast::user_val>(lex->next_token().text, val_type::string,
                                                   tok.location);
        case lexer::token_type::character:
            return std::make_unique<ast::user_val>(lex->next_token().text, val_type::character,
                                                   tok.location);
        case lexer::token_type::integer:
            return std::make_unique<ast::user_val>(lex->next_token().text, val_type::integer,
                                                   tok.location);
        case lexer::token_type::floating:
            return std::make_unique<ast::user_val>(lex->next_token().text, val_type::floating,
                                                   tok.location);
        case lexer::token_type::boolean:
            return std::make_unique<ast::user_val>(lex->next_token().text, val_type::boolean,
                                                   tok.location);
        case lexer::token_type::null:
            return std::make_unique<ast::user_val>(lex->next_token().text, val_type::null,
                                                   tok.location);
        default:
            assert(false);
        }
    }

    // if expression
    if (tok == lexer::token_type::if_) { return parse_if_expression(); }

    if (tok != lexer::token_type::identifier) {
        print_error(tok.location, "Expected an identifier, `(`, `if`, or a literal; Found ",
                    tok.text);
        lex->next_token();
        return nullptr;
    }

    auto id = lex->next_token();
    switch (auto next = lex->peek_token(); next.type) {
    // function call
    case lexer::token_type::lparen:
        return std::make_unique<ast::func_call_expr>(parse_func_call(std::move(id)), tok.location);

    // struct initialization
    case lexer::token_type::lbrace:
        return parse_struct_init(std::move(id.text), tok.location);

    // some variable
    default:
        return std::make_unique<ast::user_val>(std::move(id.text), val_type::identifier,
                                               tok.location);
    }
}

ast::func_call_data parser::parse_func_call(std::optional<lexer::token> func_name) {
    auto name = [&] {
        if (func_name.has_value()) {
            // we have already taken the function name
            assert(lex->next_token() == lexer::token_type::lparen);
            return func_name.value();
        }

        // we need to take the function name
        assert(lex->peek_token() == lexer::token_type::identifier);
        auto name = lex->next_token();
        expect_token(lexer::token_type::lparen, "(");
        return name;
    }();

    // we have already taken the lparen
    std::vector<ast::expr_ptr> args;
    while (lex->peek_token() != lexer::token_type::rparen and lex->has_more_tokens()) {
        args.push_back(parse_expression());
        switch (lex->peek_token().type) {
        case lexer::token_type::rparen:
            break;
        case lexer::token_type::comma:
            lex->next_token();
            break;
        default: {
            auto tok = lex->next_token();
            print_error(tok.location, "Expected a `)` or a `,`; Found ", tok.text);
        } break;
        }
    }

    expect_token(lexer::token_type::rparen, ")");

    return {std::move(name.text), std::move(args), name.location};
}

std::unique_ptr<ast::struct_init> parser::parse_struct_init(std::string && type_name,
                                                            Location loc) {

    auto lbrace = lex->consume_if(lexer::token_type::lbrace);
    assert(lbrace.has_value());

    std::vector<std::pair<std::string, ast::expr_ptr>> initializers;
    while (not lex->consume_if(lexer::token_type::rbrace).has_value() and lex->has_more_tokens()) {
        auto field_name = lex->next_token();
        if (field_name != lexer::token_type::identifier) {
            print_error(field_name.location, "Expected an identifier; Found ", field_name.text);
        }

        expect_token(lexer::token_type::equal, "=");

        auto expr = parse_expression();

        initializers.emplace_back(field_name.text, std::move(expr));

        switch (lex->peek_token().type) {
        case lexer::token_type::comma:
        case lexer::token_type::semi:
            lex->next_token();
            [[fallthrough]];
        case lexer::token_type::identifier:
        case lexer::token_type::rbrace:
            break;
        default: {
            auto tok = lex->next_token();
            print_error(tok.location, "Expected an identifier, `,`, `;`, or `}`; Found ", tok.text);
        } break;
        }
    }

    return std::make_unique<ast::struct_init>(std::move(type_name), std::move(initializers), loc);
}

template<typename... Args>
void parser::print_error(Location loc, Args... args) {
    static_assert(sizeof...(args) > 0);

    std::stringstream error_line;
    error_line << lex->module_name() << ':' << loc << ": ";
    (error_line << ... << args);

    error_printout.emplace_back(error_line.str());
}

void parser::expect_token(lexer::token_type tok_type, std::string text) {
    if (auto tok = lex->peek_token(); tok != tok_type) {
        print_error(tok.location, "Expected `", std::move(text), "`; Found ", tok.text);
    } else {
        lex->next_token();
    }
}
