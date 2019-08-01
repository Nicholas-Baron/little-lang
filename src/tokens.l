%{

#include "node.hpp"
#include "parser.hpp"

#include <string>

#define save_tok yylval.string = new std::string{yytext, static_cast<size_t>(yyleng)}
#define token(t) (yylval.token = t)

extern "C" int yywrap(){ return 1; /* To prevent reading a second file */ }

%}

%option nodefault
%option nounput
%option header="tokens.hpp"

%%

[ \t\n]		; /* Whitespace agnostic */

(#|\/\/|[Cc]omment)[^\n\r]*		; /* Comments are removed */

	/* Comparison Symbols */

"!="			return token(T_NE);
"<"				return token(T_LT);
"<="			return token(T_LE);
">"				return token(T_GT);
">="			return token(T_GE);

	/* Paired Symbols */

"("				return token(T_LPAREN);
")"				return token(T_RPAREN);
"{"				return token(T_LBRACE);
"}"				return token(T_RBRACE);
"["				return token(T_LBRACK);
"]"				return token(T_RBRACK);

	/* Mathematical Symbols */

"+"				return token(T_PLUS);	
"-"				return token(T_MINUS);
"/"				return token(T_DIV);
"*"				return token(T_MULT);
"%"				return token(T_MOD);

	/* Miscellaneous Symbols */

"::"			return token(T_SCOPE);
","				return token(T_COMMA);
";"				return token(T_SEMI);

	/* Symbols w/ word alternates */

"and"|"&&"		return token(T_AND);
"or"|"||"		return token(T_OR);
"not"|"!"		return token(T_NOT);
"equals"|"=="	return token(T_EQ);
"equal"|"="		return token(T_ASSIGN);
"is"			return token(T_IS);

	/* Reserved words */

"namespace"		return token(T_NAMESPACE);
"global"		return token(T_GLOBAL);
"return"|"ret"	return token(T_RET);
"let"			return token(T_LET);
"if"			return token(T_IF);
"else"			return token(T_ELSE);


	/* Literals */

0|(-?[1-9][0-9_]*)|(0x[0-9A-Fa-f]+)		save_tok; return T_INT;
-?[0-9]*\.[0-9]+						save_tok; return T_FLOAT;
\"(\\.|[^\"])*\"						save_tok; return T_STRING;
'(.|\\.)'								save_tok; return T_CHAR;
[Tt]rue|TRUE|[Ff]alse|FALSE				save_tok; return T_BOOL;

	/* Identifier */

[a-zA-Z][a-zA-Z0-9_]+					save_tok; return T_IDENT;


	/* Error: Could not parse */
.				printf("Unknown token!\n"); yyterminate();

%%
