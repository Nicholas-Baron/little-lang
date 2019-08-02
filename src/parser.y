%{
	#include <iostream>

	extern int yylex();
	extern const char* yytext;
	extern int yylineno;	
	inline void yyerror(const char* const msg){ std::cerr << "Error on line " << yylineno << ": " << msg << "\nText: " << yytext << std::endl; }

%}

%glr-parser

%union{
	int token;
	std::string* string;



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

%precedence T_OR T_AND
%precedence T_NE T_EQ T_LT T_LE T_GT T_GE
%precedence T_PLUS T_MINUS
%precedence T_MULT T_MOD T_DIV
%precedence T_NOT

%start program

// {$$ = $1;} is already provided.

%%

program : top_lvl_seq function 

top_lvl_seq : %empty
			| top_lvl_item top_lvl_seq
			;

top_lvl_item : global
			 | function
			 ;

global : T_GLOBAL typed_var initialization T_SEMI
	   ;

function : func_header statement 
		 ;

func_header : T_IDENT func_sig
			| func_sig T_IDENT
			;

func_sig : T_IDENT param_group
		 ;

param_group : T_LPAREN T_RPAREN
			| T_LPAREN param_list T_RPAREN
			;

param_list : typed_var
		   | param_list T_COMMA typed_var
		   ;

typed_var : T_IDENT T_IDENT
		  | T_IDENT T_IS T_IDENT
		  ;

statement : T_LBRACE statement_seq T_RBRACE
		  | action T_SEMI
		  | conditional
		  ;

statement_seq : %empty
			  | statement_seq statement
			  ;

action : T_RET expr
	   | T_RET
	   | func_call
	   | T_LET T_IDENT initialization
	   | T_LET typed_var initialization
	   ;

conditional : T_IF expr T_LBRACE statement_seq T_RBRACE else_block
			| T_IF expr statement
			;

else_block : T_ELSE T_LBRACE statement_seq T_RBRACE
		   | T_ELSE conditional
		   ;

initialization : T_ASSIGN expr | T_LBRACE expr T_RBRACE ;

expr : T_IDENT
	 | func_call
	 | T_INT
	 | T_FLOAT
	 | T_CHAR
	 | T_BOOL
	 | T_LPAREN expr T_RPAREN
	 | T_NOT expr
	 | expr binary_op expr
	 ;

func_call : T_IDENT arg_group
		  | T_LPAREN T_IDENT arg_list T_RPAREN
		  | T_IDENT expr
		  ;

arg_group : T_LPAREN arg_list T_RPAREN
		  | T_LPAREN T_RPAREN
		  ;

arg_list : expr
		 | arg_list T_COMMA expr
		 ;

binary_op: T_AND 
		 | T_OR 
		 | T_EQ 
		 | T_NE 
		 | T_LE 
		 | T_LT 
		 | T_GE 
		 | T_GT 
		 | T_PLUS 
		 | T_MINUS
		 | T_MULT 
		 | T_MOD 
		 | T_DIV 
		 ;

%%
