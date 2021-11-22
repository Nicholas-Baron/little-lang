#include "token_to_string.hpp"

#include "parser.hpp" // token names (should not be needed here)

// TODO: Test this?
[[nodiscard]] std::string tok_to_string(int tok) {
    switch (tok) {
    default:
        return "[Internal Bison Value]";
    case T_EQ:
        return "T_EQ";
    case T_NE:
        return "T_NE";
    case T_LT:
        return "T_LT";
    case T_GT:
        return "T_GT";
    case T_LE:
        return "T_LE";
    case T_GE:
        return "T_GE";
    case T_LPAREN:
        return "T_LPAREN";
    case T_RPAREN:
        return "T_RPAREN";
    case T_LBRACE:
        return "T_LBRACE";
    case T_RBRACE:
        return "T_RBRACE";
    case T_LBRACK:
        return "T_LBRACK";
    case T_RBRACK:
        return "T_RBRACK";
    case T_PLUS:
        return "T_PLUS";
    case T_MINUS:
        return "T_MINUS";
    case T_DIV:
        return "T_DIV";
    case T_MULT:
        return "T_MULT";
    case T_MOD:
        return "T_MOD";
    case T_COMMA:
        return "T_COMMA";
    case T_IS:
        return "T_IS";
    case T_SEMI:
        return "T_SEMI";
    case T_DOT:
        return "T_DOT";
    case T_RET:
        return "T_RET";
    case T_IF:
        return "T_IF";
    case T_ELSE:
        return "T_ELSE";
    case T_LET:
        return "T_LET";
    case T_CONST:
        return "T_CONST";
    case T_AND:
        return "T_AND";
    case T_OR:
        return "T_OR";
    case T_NOT:
        return "T_NOT";
    case T_ASSIGN:
        return "T_ASSIGN";
    case T_PROC:
        return "T_PROC";
    case T_IDENT:
        return "T_IDENT";
    case T_INT:
        return "T_INT";
    case T_CHAR:
        return "T_CHAR";
    case T_BOOL:
        return "T_BOOL";
    case T_STRING:
        return "T_STRING";
    case T_FLOAT:
        return "T_FLOAT";
    case T_PRIM_TYPE:
        return "T_PRIM_TYPE";
    }
}
