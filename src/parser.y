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

conditional : T_IF boolean_expr T_LBRACE statement_seq T_RBRACE else_block
			| T_IF boolean_expr statement
			;

else_block : %empty
		   | T_ELSE T_LBRACE statement_seq T_RBRACE
		   | T_ELSE conditional
		   ;

initialization : T_ASSIGN expr | T_LBRACE expr T_RBRACE ;

expr : math_expr | bool_expr;

math_expr : T_IDENT
		  | T_INT
		  | T_FLOAT
		  | func_call
		  | T_LPAREN math_expr T_RPAREN
		  | math_expr binary_op_rhs
		  ;

bool_expr : T_IDENT
		  | T_BOOL
		  | func_call
		  | T_NOT bool_expr
		  | T_LPAREN bool_expr T_RPAREN
		  | math_expr compare_op_rhs
		  | bool_expr bool_op_rhs
		  ;

func_call : T_IDENT T_LPAREN arg_list T_RPAREN
		  | T_LPAREN T_IDENT arg_list T_RPAREN
		  | T_IDENT T_LPAREN T_RPAREN
		  | T_IDENT expr
		  ;

arg_list : expr
		 | arg_list T_COMMA expr
		 ;

bool_op_rhs : T_AND bool_expr
			| T_OR bool_expr
			;

compare_op_rhs : T_EQ math_expr
			   | T_NE math_expr
			   | T_LE math_expr
			   | T_LT math_expr
			   | T_GE math_expr
			   | T_GT math_expr
			   ;

binary_op_rhs : T_PLUS math_expr
			  | T_MINUS math_expr
			  | T_MULT math_expr
			  | T_MOD math_expr
			  | T_DIV math_expr
			  ;

%%
