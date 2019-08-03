%{
	#include <iostream>

	#include "nodes.hpp"

	extern int yylex();
	extern const char* yytext;
	extern int yylineno;	
	inline void yyerror(const char* const msg){ 
		std::cerr << "Error on line " << yylineno << ": " << msg << "\nText: " << yytext << std::endl; 
	}

	Top_Level_Seq * module;
%}

%glr-parser

%union{
	int token;
	std::string * string;

	Typed_Var * var_with_type;
	Top_Level_Seq * top_lvl_items;
	
	std::vector<Typed_Var>* params;
}

// Token definitions

%token <string> T_IDENT T_INT T_CHAR T_BOOL T_STRING T_FLOAT T_PRIM_TYPE 	// Regexes
%token <token>	T_EQ T_NE T_LT T_GT T_LE T_GE 								// Comparisons
%token <token>	T_LPAREN T_RPAREN T_LBRACE T_RBRACE T_LBRACK T_RBRACK		// Paired symbols
%token <token>	T_PLUS T_MINUS T_DIV T_MULT T_MOD 							// Math symbols
%token <token>	T_SCOPE T_COMMA T_IS T_SEMI									// Misc symbols
%token <token>	T_NAMESPACE T_GLOBAL T_RET T_IF T_ELSE T_LET				// Reserved words
%token <token>	T_AND T_OR T_NOT 											// Boolean operators
%token <token>	T_ASSIGN T_PROC	

// Types for non-terminals
%nterm <string> literal type ret_type
%type <top_lvl_items> top_lvl_seq
%type <var_with_type> typed_var
%type <params> param_list param_group

%start program

// {$$ = $1;} is already provided.

%%

program : top_lvl_seq function 

top_lvl_seq : %empty { $$ = new Top_Level_Seq(); }
			| top_lvl_seq top_lvl_item 
			;

top_lvl_item : global
			 | function
			 ;

global : T_GLOBAL typed_var initialization T_SEMI
	   ;

function : func_header statement 
		 ;

func_header : ret_type func_sig
			| func_sig ret_type 
			;

ret_type : type | T_PROC { $$ = new std::string{"proc"}; } ;

func_sig : T_IDENT param_group ;

param_group : T_LPAREN T_RPAREN { $$ = new std::vector<Typed_Var>{}; }
			| T_LPAREN param_list T_RPAREN { $$ = $2; }
			;

param_list : typed_var { $$ = new std::vector<Typed_Var>{}; $$->push_back(std::move(*$1)); delete $1; }
		   | param_list T_COMMA typed_var { $$ = $1; $$->push_back(std::move(*$3)); delete $3; }
		   ;

typed_var : type T_IDENT 		{ $$ = new Typed_Var(std::move(*$2), std::move(*$1)); delete $1; delete $2; }
		  | T_IDENT T_IS type 	{ $$ = new Typed_Var(std::move(*$1), std::move(*$3)); delete $1; delete $3; }
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

expr : logic_or_expr ;

primary_expr : T_IDENT
			 | func_call
			 | literal
			 | T_LPAREN expr T_RPAREN
			 ;

literal : T_INT | T_FLOAT | T_CHAR | T_BOOL | T_STRING ; 

unary_expr : primary_expr | T_NOT primary_expr ; 

multiply_expr : unary_expr 
			  | multiply_expr T_MULT unary_expr
			  | multiply_expr T_MOD unary_expr
			  | multiply_expr T_DIV unary_expr
			  ;

additive_expr : multiply_expr
			  | additive_expr T_PLUS multiply_expr
			  | additive_expr T_MINUS multiply_expr
			  ;

relation_expr : additive_expr
			  | additive_expr T_LE additive_expr
			  | additive_expr T_LT additive_expr
			  | additive_expr T_GE additive_expr
			  | additive_expr T_GT additive_expr
			  ;

equality_expr : relation_expr
			  | relation_expr T_EQ relation_expr
			  | relation_expr T_NE relation_expr
			  ;

logic_and_expr : equality_expr
			   | logic_and_expr T_AND equality_expr
			   ;

logic_or_expr : logic_and_expr
			  | logic_or_expr T_OR logic_and_expr
			  ;

func_call : T_IDENT arg_group
		  | T_LPAREN T_IDENT arg_list T_RPAREN
		  | T_IDENT func_call
		  | T_IDENT literal
		  ;

arg_group : T_LPAREN arg_list T_RPAREN
		  | T_LPAREN T_RPAREN
		  ;

arg_list : expr
		 | arg_list T_COMMA expr
		 ;

type : T_PRIM_TYPE | T_IDENT ;

%%
