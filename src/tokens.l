%{

#include "nodes.hpp"
#include "parser.hpp"

#include <string>

#define save_tok yylval.string = new std::string{yytext, static_cast<size_t>(yyleng)}
#define token(t) (yylval.token = t)
#define YY_USER_ACTION record_location();

extern "C" int yywrap(){ return 1; /* To prevent reading a second file */ }
int yycolumn = 1;

extern "C" void record_location(){

    yylloc.first_line = yylloc.last_line = yylineno;
    yylloc.first_column = yycolumn;
    yylloc.last_column = yycolumn + (yyleng - 1);
    yycolumn += yyleng;

}

%}

%option yylineno
%option warn
%option nodefault
%option noinput
%option nounput
%option header="tokens.hpp"

number -?([1-9][0-9]*|0)

%%

[ \t\n]        ; /* Whitespace agnostic */

(#|\/\/|[Cc]omment)[^\n\r]*        ; /* Comments are removed */

    /* Comparison Symbols */

"!="            return token(T_NE);
"<"             return token(T_LT);
"<="            return token(T_LE);
">"             return token(T_GT);
">="            return token(T_GE);

    /* Paired Symbols */

"("             return token(T_LPAREN);
")"             return token(T_RPAREN);
"{"             return token(T_LBRACE);
"}"             return token(T_RBRACE);
"["             return token(T_LBRACK);
"]"             return token(T_RBRACK);

    /* Mathematical Symbols */

"+"             return token(T_PLUS);
"-"             return token(T_MINUS);
"/"             return token(T_DIV);
"*"             return token(T_MULT);
"%"             return token(T_MOD);

    /* Miscellaneous Symbols */

","             return token(T_COMMA);
";"             return token(T_SEMI);
"."             return token(T_DOT);

    /* Symbols w/ word alternates */

"and"|"&&"      return token(T_AND);
"or"|"||"       return token(T_OR);
"not"|"!"       return token(T_NOT);
"equals"|"=="   return token(T_EQ);
"equal"|"="     return token(T_ASSIGN);
"is"|":"        return token(T_IS);

    /* Reserved words */

"return"|"ret"  return token(T_RET);
"let"           return token(T_LET);
"if"            return token(T_IF);
"else"          return token(T_ELSE);
"proc"          return token(T_PROC);
"const"         return token(T_CONST);
"int"|"bool"|"float"|"char"|"auto" save_tok; return T_PRIM_TYPE;

    /* Literals */

0|({number})|(0x[0-9A-Fa-f]+) save_tok; return T_INT;
{number}\.([0-9]*[1-9]|0)     save_tok; return T_FLOAT;
\"(\\.|[^\"])*\"              save_tok; return T_STRING;
'(.|\\.)'                     save_tok; return T_CHAR;
[Tt]rue|TRUE|[Ff]alse|FALSE   save_tok; return T_BOOL;

    /* Identifier */

[a-zA-Z][a-zA-Z0-9_]+         save_tok; return T_IDENT;


    /* Error: Could not parse */
.                printf("Unknown text on line %d: %s!\n", yylineno, yytext); yyterminate();

%%
