%{
    #include <iostream>
    #include <memory>

    #include "location.hpp"

    extern int yylex();
    extern const char* yytext;
    extern int yylineno;
    static void yyerror(const char* const msg){
        std::cerr << "Error on line " << yylineno << ": " << msg << "\nText: " << yytext << std::endl;
    }

%}

%locations

%code requires {
    #include <string>
    #include <vector>

    #include <ast/nodes.hpp>
}

%code provides {
    static_assert(sizeof(YYSTYPE) <= sizeof(int *), "The Bison union is not of trivial size.");
}

%code {
    using namespace ast;
    std::unique_ptr<top_level_sequence> module;

    [[nodiscard]] static Location make_loc(const YYLTYPE& yy_loc){
        return Location{
            yy_loc.first_line, yy_loc.first_column,
            yy_loc.last_line, yy_loc.last_column
        };
    }

    #define set_loc(item, loc) (item)->set_location(make_loc(loc))
}

%union{
    int token;
    std::string * string;
    std::vector<std::string> * strings;
    std::map<std::string, std::vector<std::string>> * imports;

    ast::expr * expression;
    ast::stmt * stmt;
    ast::top_level * top_lvl;

    ast::func_call_data * func_call;

    ast::func_decl::header * func_head;
    ast::typed_identifier * var_with_type;

    ast::stmt_sequence * stmts;
    ast::top_level_sequence * top_lvl_items;

    std::vector<ast::typed_identifier>* params;
    std::vector<ast::expr_ptr>* args;
}

// Token definitions

%token <token>    T_EQ "==" T_NE "!=" T_LT "<" T_GT ">" T_LE "<=" T_GE  ">="             // Comparisons
%token <token>    T_LPAREN "(" T_RPAREN ")" T_LBRACE "{" T_RBRACE "}" T_LBRACK T_RBRACK  // Paired symbols
%token <token>    T_PLUS "+" T_MINUS "-" T_DIV "/" T_MULT "*" T_MOD "%"                  // Math symbols
%token <token>    T_COMMA "," T_IS "is" T_SEMI ";" T_DOT "."                             // Misc symbols
%token <token>    T_RET "return" T_IF "if" T_ELSE "else" T_LET "let" T_CONST "const"     // Reserved words
%token <token>    T_AND "and" T_OR "or" T_NOT "not"                                      // Boolean operators
%token <token>    T_ASSIGN "=" T_ARROW "->" T_FROM "from" T_IMPORT "import"
%token <string>   T_IDENT T_INT T_CHAR T_BOOL T_STRING T_FLOAT T_PRIM_TYPE               // Regexes

// Types for non-terminals
%nterm <string> type ret_type
%type <strings> import_list
%type <imports> imports
%type <expression> expr literal initialization
%type <stmt> stmt else_block conditional action
%type <top_lvl> top_lvl_item function const_decl
%type <func_call> func_call
%type <func_head> func_header func_sig
%type <var_with_type> typed_var
%type <stmts> stmt_seq stmt_block
%type <top_lvl_items> top_lvl_seq
%type <params> param_list param_group
%type <args> arg_list arg_group

%start program

%left T_OR
%left T_AND
%nonassoc T_LE T_LT T_GE T_GT T_EQ T_NE
%left T_PLUS T_MINUS
%left T_MOD T_DIV T_MULT
%precedence T_NOT

// {$$ = $1;} is already provided.

%%

program : imports top_lvl_seq {
            module = std::unique_ptr<top_level_sequence>($2);
            module->imports = std::move(*$1);
            delete $1;
            set_loc(module, @$);
        }
        ;

imports : %empty { $$ = new std::map<std::string, std::vector<std::string>>; }
        | imports T_FROM T_STRING T_IMPORT import_list {
            $$ = $1; $$->emplace(std::move(*$3), std::move(*$5)); delete $3; delete $5;
        }
        ;

import_list : T_IDENT { $$ = new std::vector{std::move(*$1)}; delete $1; }
            | import_list T_COMMA T_IDENT { $$ = $1; $$->push_back(std::move(*$3)); delete $3; }
            ;

top_lvl_seq : top_lvl_item { $$ = new top_level_sequence{$1}; set_loc($$, @$); }
            | top_lvl_seq top_lvl_item {
                $$ = $1; $$->append($2); set_loc($$, @$);
            }
            ;

top_lvl_item : function | const_decl ;

const_decl : T_CONST typed_var initialization { $$ = new const_decl{std::move(*$2), $3}; delete $2; set_loc($$, @$); }
         ;

function : func_header stmt {
             $$ = new func_decl{std::move(*$1), $2}; delete $1; set_loc($$, @$);
         }
         | func_header T_ASSIGN expr {
            $$ = new func_decl{std::move(*$1), new return_stmt{$3}}; delete $1; set_loc($$, @$);
         }
         ;

func_header : func_sig ret_type { $$ = $1; $$->set_ret_type(std::move(*$2)); delete $2; set_loc($$, @$); }
            ;

ret_type : T_ARROW type { $$ = $2; }
         | %empty { $$ = new std::string{"proc"}; }
         ;

func_sig : T_IDENT param_group { $$ = new func_decl::header{std::move(*$1), std::move(*$2)}; delete $1; delete $2; set_loc($$, @$); } ;

param_group : T_LPAREN T_RPAREN { $$ = new std::vector<typed_identifier>; }
            | T_LPAREN param_list T_RPAREN { $$ = $2; }
            ;

param_list : typed_var { $$ = new std::vector<typed_identifier>{std::move(*$1)}; delete $1; }
           | param_list T_COMMA typed_var { $$ = $1; $$->push_back(std::move(*$3)); delete $3; }
           ;

typed_var : type T_IDENT         { $$ = new typed_identifier(std::move(*$2), std::move(*$1)); delete $1; delete $2; set_loc($$, @$); }
          | T_IDENT T_IS type     { $$ = new typed_identifier(std::move(*$1), std::move(*$3)); delete $1; delete $3; set_loc($$, @$); }
          ;

stmt : stmt_block { $$ = dynamic_cast<stmt *>($1); } | action T_SEMI | conditional ;

stmt_block : T_LBRACE stmt_seq T_RBRACE { $$ = $2; set_loc($$, @$); } ;

stmt_seq : %empty { $$ = new stmt_sequence; set_loc($$, @$); }
         | stmt_seq stmt { $$ = $1; $$->append($2); set_loc($$, @$); }
         ;

action : T_RET expr { $$ = new return_stmt($2); set_loc($$, @$);}
       | T_RET { $$ = new return_stmt; set_loc($$, @$);}
       | func_call { $$ = new func_call_stmt(std::move(*$1)); delete $1; set_loc($$, @$);}
       | T_LET T_IDENT initialization { $$ = new let_stmt(std::move(*$2), $3); delete $2; set_loc($$, @$); }
       | T_LET typed_var initialization { $$ = new let_stmt(std::move(*$2), $3); delete $2; set_loc($$, @$); }
       ;

conditional : T_IF expr stmt_block else_block { $$ = new if_stmt($2, $3, $4); set_loc($$, @$); }
            | T_IF expr stmt    { $$ = new if_stmt($2, $3, nullptr); set_loc($$, @$); }
            ;

else_block : T_ELSE stmt_block { $$ = $2; set_loc($$, @$);  }
           | T_ELSE conditional { $$ = $2; set_loc($$, @$);  }
           ;

initialization : T_ASSIGN expr { $$ = $2;  set_loc($$, @$); }
               | T_LBRACE expr T_RBRACE { $$ = $2; set_loc($$, @$);  }
               ;

literal : T_INT { $$ = new user_val(std::move(*$1), user_val::value_type::integer); delete $1; set_loc($$, @$); }
        | T_FLOAT { $$ = new user_val(std::move(*$1), user_val::value_type::floating); delete $1; set_loc($$, @$); }
        | T_CHAR { $$ = new user_val(std::move(*$1), user_val::value_type::character); delete $1; set_loc($$, @$); }
        | T_BOOL { $$ = new user_val(std::move(*$1), user_val::value_type::boolean); delete $1; set_loc($$, @$); }
        | T_STRING { $$ = new user_val(std::move(*$1), user_val::value_type::string); delete $1; set_loc($$, @$); }
        ;

expr  : T_IDENT { $$ = new user_val(std::move(*$1), user_val::value_type::identifier); delete $1; set_loc($$, @$);  }
      | func_call { $$ = new func_call_expr(std::move(*$1)); delete $1; set_loc($$, @$);  }
      | literal
      | T_LPAREN expr T_RPAREN { $$ = $2; set_loc($$, @$);  }
      | T_NOT expr { $$ = new unary_expr($1, $2); set_loc($$, @$);  }
      | T_MINUS expr { $$ = new unary_expr($1, $2); set_loc($$, @$); } %prec T_NOT
      | expr T_MULT expr { $$ = new binary_expr($1, $2, $3); set_loc($$, @$);  }
      | expr T_MOD  expr { $$ = new binary_expr($1, $2, $3); set_loc($$, @$);  }
      | expr T_DIV  expr { $$ = new binary_expr($1, $2, $3); set_loc($$, @$);  }
      | expr T_PLUS  expr { $$ = new binary_expr($1, $2, $3); set_loc($$, @$);  }
      | expr T_MINUS expr { $$ = new binary_expr($1, $2, $3); set_loc($$, @$);  }
      | expr T_LE expr { $$ = new binary_expr($1, $2, $3); set_loc($$, @$);  }
      | expr T_LT expr { $$ = new binary_expr($1, $2, $3); set_loc($$, @$);  }
      | expr T_GE expr { $$ = new binary_expr($1, $2, $3); set_loc($$, @$);  }
      | expr T_GT expr { $$ = new binary_expr($1, $2, $3); set_loc($$, @$);  }
      | expr T_EQ expr { $$ = new binary_expr($1, $2, $3); set_loc($$, @$);  }
      | expr T_NE expr { $$ = new binary_expr($1, $2, $3); set_loc($$, @$);  }
      | expr T_AND expr { $$ = new binary_expr($1, $2, $3); set_loc($$, @$);  }
      | expr T_OR expr { $$ = new binary_expr($1, $2, $3); set_loc($$, @$);  }
      ;

func_call : T_IDENT arg_group {
            $$ = new func_call_data(std::move(*$1), std::move(*$2)); delete $1; delete $2;
          }
          | T_IDENT T_DOT func_call {
            std::vector<std::unique_ptr<expr>> args;
            args.emplace_back(std::make_unique<func_call_expr>(std::move(*$3)));
            $$ = new func_call_data{std::move(*$1), std::move(args)};
            delete $1;
            delete $3;
          }
          ;

arg_group : T_LPAREN arg_list T_RPAREN { $$ = $2; }
          | T_LPAREN T_RPAREN { $$ = new std::vector<std::unique_ptr<expr>>; }
          ;

arg_list : expr {
            $$ = new std::vector<std::unique_ptr<expr>>;
            $$->emplace_back($1);
         }
         | arg_list T_COMMA expr { $$ = $1; $$->emplace_back($3); }
         ;

type : T_PRIM_TYPE | T_IDENT ;

%%
