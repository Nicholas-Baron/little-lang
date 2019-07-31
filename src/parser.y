%{
	#include <iostream>

	inline void yyerror(const char* const msg){ std::cerr << msg << std::endl; }

	extern int yylex();

	template<typename T>
	using ptr = std::unique_ptr<T>;
%}

%union{
	int token;
	ptr<std::string> string;



}

// Token definitions

%token <string> T_IDENT T_INT T_CHAR T_BOOL T_STRING T_FLOAT			// Regexes
%token <token>	T_EQ T_NE T_LT T_GT T_LE T_GE 							// Comparisons
%token <token>	T_LPAREN T_RPAREN T_LBRACE T_RBRACE T_LBRACK T_RBRACK	// Paired symbols
%token <token>	T_PLUS T_MINUS T_DIV T_MULT T_MOD 						// Math symbols
%token <token>	T_SCOPE T_COMMA T_IS T_SEMI								// Misc symbols
%token <token>	T_NAMESPACE T_GLOBAL T_RET T_IF T_ELSE T_LET			// Reserved words
%token <token>	T_AND T_OR T_NOT 										// Boolean operators
%token <token>	T_ASSIGN

// Types for non-terminals

// Precedence (lowest = first)

%right T_ASSIGN
%left T_OR T_AND
%left T_NE T_EQ T_LT T_LE T_GT T_GE
%left T_PLUS T_MINUS
%left T_MULT T_MOD T_DIV
%right T_NOT

%start program

// {$$ = $1;} is already provided.

%%


%%
