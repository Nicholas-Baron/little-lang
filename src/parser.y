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
%locations

%union{
	int token;
	std::string * string;

	Expression * expression;
	Statement * stmt;
	Top_Level * top_lvl;

	FunctionCall * func_call;

	Func_Header * func_head;
	Typed_Var * var_with_type;
	
	Statement_Seq * statements;
	Top_Level_Seq * top_lvl_items;
	
	std::vector<Typed_Var>* params;
	std::vector<Expression*>* args;
}

// Token definitions

%token <token>	T_EQ T_NE T_LT T_GT T_LE T_GE 								// Comparisons
%token <token>	T_LPAREN T_RPAREN T_LBRACE T_RBRACE T_LBRACK T_RBRACK		// Paired symbols
%token <token>	T_PLUS T_MINUS T_DIV T_MULT T_MOD 							// Math symbols
%token <token>	T_SCOPE T_COMMA T_IS T_SEMI									// Misc symbols
%token <token>	T_NAMESPACE T_RET T_IF T_ELSE T_LET							// Reserved words
%token <token>	T_AND T_OR T_NOT 											// Boolean operators
%token <token>	T_ASSIGN T_PROC	
%token <string> T_IDENT T_INT T_CHAR T_BOOL T_STRING T_FLOAT T_PRIM_TYPE 	// Regexes

// Types for non-terminals
%nterm <string> literal type ret_type
%type <expression> expr logic_or_expr logic_and_expr primary_expr equality_expr
%type <expression> unary_expr multiply_expr relation_expr additive_expr 
%type <expression> initialization
%type <stmt> statement else_block conditional action
%type <top_lvl> top_lvl_item function
%type <func_call> func_call
%type <func_head> func_header func_sig
%type <var_with_type> typed_var
%type <statements> statement_seq statement_block
%type <top_lvl_items> top_lvl_seq
%type <params> param_list param_group
%type <args> arg_list arg_group

%start program

// {$$ = $1;} is already provided.

%%

program : top_lvl_seq function { module = $1; module->append($2); }

top_lvl_seq : %empty { $$ = new Top_Level_Seq(); }
			| top_lvl_seq top_lvl_item { $$ = $1; $$->append($2); }
			;

top_lvl_item : function ;

function : func_header statement { $$ = new Function{std::move(*$1), $2}; delete $1; std::cout << "Found a function on line " << @1.first_line << std::endl; }
		 ;

func_header : ret_type func_sig { $$ = $2; $$->set_ret_type(std::move(*$1)); delete $1; }
			| func_sig ret_type { $$ = $1; $$->set_ret_type(std::move(*$2)); delete $2; }
			;

ret_type : type | T_PROC { $$ = new std::string{"proc"}; } ;

func_sig : T_IDENT param_group { $$ = new Func_Header{std::move(*$1), std::move(*$2)}; delete $1; delete $2; } ;

param_group : T_LPAREN T_RPAREN { $$ = new std::vector<Typed_Var>{}; }
			| T_LPAREN param_list T_RPAREN { $$ = $2; }
			;

param_list : typed_var { $$ = new std::vector<Typed_Var>{}; $$->push_back(std::move(*$1)); delete $1; }
		   | param_list T_COMMA typed_var { $$ = $1; $$->push_back(std::move(*$3)); delete $3; }
		   ;

typed_var : type T_IDENT 		{ $$ = new Typed_Var(std::move(*$2), std::move(*$1)); delete $1; delete $2; }
		  | T_IDENT T_IS type 	{ $$ = new Typed_Var(std::move(*$1), std::move(*$3)); delete $1; delete $3; }
		  ;

statement : statement_block { $$ = dynamic_cast<Statement *>($1); } | action T_SEMI | conditional ;

statement_block : T_LBRACE statement_seq T_RBRACE { $$ = $2; } ;

statement_seq : %empty { $$ = new Statement_Seq{}; }
			  | statement_seq statement { $$ = $1; $$->append($2); }
			  ;

action : T_RET expr { $$ = new Return_Statement($2); }
	   | T_RET { $$ = new Return_Statement(); }
	   | func_call { $$ = dynamic_cast<Statement *>($1); }
	   | T_LET T_IDENT initialization { $$ = new Let_Statement(std::move(*$2), $3); delete $2; }
	   | T_LET typed_var initialization { $$ = new Let_Statement(std::move(*$2), $3); delete $2; }
	   ;

conditional : T_IF expr statement_block else_block { $$ = new If_Statement($2, $3, $4); }
			| T_IF expr statement	{ $$ = new If_Statement($2, $3, nullptr); }
			;

else_block : T_ELSE statement_block { $$ = $2; }
		   | T_ELSE conditional { $$ = $2; }
		   ;

initialization : T_ASSIGN expr { $$ = $2; } | T_LBRACE expr T_RBRACE { $$ = $2; };

expr : logic_or_expr ;

primary_expr : T_IDENT { $$ = new UserValue(std::move(*$1)); delete $1; }
			 | func_call { $$ = dynamic_cast<Expression*>($1); }
			 | literal { $$ = new UserValue(std::move(*$1)); delete $1; }
			 | T_LPAREN expr T_RPAREN { $$ = $2; }
			 ;

literal : T_INT | T_FLOAT | T_CHAR | T_BOOL | T_STRING ; 

unary_expr : primary_expr | T_NOT primary_expr { $$ = new UnaryExpression($1, $2); } ; 

multiply_expr : unary_expr 
			  | multiply_expr T_MULT unary_expr { $$ = new BinaryExpression($1, $2, $3); }
			  | multiply_expr T_MOD unary_expr { $$ = new BinaryExpression($1, $2, $3); }
			  | multiply_expr T_DIV unary_expr { $$ = new BinaryExpression($1, $2, $3); }
			  ;

additive_expr : multiply_expr
			  | additive_expr T_PLUS multiply_expr { $$ = new BinaryExpression($1, $2, $3); }
			  | additive_expr T_MINUS multiply_expr { $$ = new BinaryExpression($1, $2, $3); }
			  ;

relation_expr : additive_expr
			  | additive_expr T_LE additive_expr { $$ = new BinaryExpression($1, $2, $3); }
			  | additive_expr T_LT additive_expr { $$ = new BinaryExpression($1, $2, $3); }
			  | additive_expr T_GE additive_expr { $$ = new BinaryExpression($1, $2, $3); }
			  | additive_expr T_GT additive_expr { $$ = new BinaryExpression($1, $2, $3); }
			  ;

equality_expr : relation_expr
			  | relation_expr T_EQ relation_expr  { $$ = new BinaryExpression($1, $2, $3); }
			  | relation_expr T_NE relation_expr { $$ = new BinaryExpression($1, $2, $3); }
			  ;

logic_and_expr : equality_expr
			   | logic_and_expr T_AND equality_expr { $$ = new BinaryExpression($1, $2, $3); }
			   ;

logic_or_expr : logic_and_expr
			  | logic_or_expr T_OR logic_and_expr { $$ = new BinaryExpression($1, $2, $3); }
			  ;

func_call : T_IDENT arg_group { $$ = new FunctionCall(std::move(*$1), std::move(*$2)); delete $1; delete $2; }
		  | T_LPAREN T_IDENT arg_list T_RPAREN { $$ = new FunctionCall(std::move(*$2), std::move(*$3)); delete $2; delete $3; }
		  | T_IDENT func_call { $$ = new FunctionCall{std::move(*$1), {$2}}; delete $1; }
		  | T_IDENT literal { $$ = new FunctionCall{std::move(*$1), {new UserValue(std::move(*$2))}}; delete $1; delete $2;}
		  ;

arg_group : T_LPAREN arg_list T_RPAREN { $$ = $2; }
		  | T_LPAREN T_RPAREN { $$ = new std::vector<Expression*>{}; }
		  ;

arg_list : expr { $$ = new std::vector{$1}; }
		 | arg_list T_COMMA expr { $$ = $1; $$->push_back($3); }
		 ;

type : T_PRIM_TYPE | T_IDENT ;

%%
