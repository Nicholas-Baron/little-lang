%{
    #include <iostream>
    #include <memory>

    #include "location.hpp"
    #include "nodes.hpp"

    extern int yylex();
    extern const char* yytext;
    extern int yylineno;
    inline void yyerror(const char* const msg){
        std::cerr << "Error on line " << yylineno << ": " << msg << "\nText: " << yytext << std::endl;
    }

    std::unique_ptr<Top_Level_Seq> module;
%}

%locations

%code provides {

    [[nodiscard]] inline Location make_loc(const YYLTYPE& yy_loc){
        return Location{
            yy_loc.first_line, yy_loc.first_column,
            yy_loc.last_line, yy_loc.last_column
        };
    }

    static_assert(sizeof(YYSTYPE) <= sizeof(int *), "The Bison union is not of trivial size.");

    #define set_loc(item, loc) (item)->set_location(make_loc(loc))
}

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
    std::vector<std::unique_ptr<Expression>>* args;
}

// Token definitions

%token <token>    T_EQ "==" T_NE "!=" T_LT "<" T_GT ">" T_LE "<=" T_GE  ">="                // Comparisons
%token <token>    T_LPAREN "(" T_RPAREN ")" T_LBRACE "{" T_RBRACE "}" T_LBRACK T_RBRACK    // Paired symbols
%token <token>    T_PLUS "+" T_MINUS "-" T_DIV "/" T_MULT "*" T_MOD "%"                    // Math symbols
%token <token>    T_COMMA "," T_IS "is" T_SEMI ";" T_DOT "."                                // Misc symbols
%token <token>    T_RET "return" T_IF "if" T_ELSE "else" T_LET "let"                        // Reserved words
%token <token>    T_AND "and" T_OR "or" T_NOT "not"                                         // Boolean operators
%token <token>    T_ASSIGN "=" T_PROC    "proc"
%token <string> T_IDENT T_INT T_CHAR T_BOOL T_STRING T_FLOAT T_PRIM_TYPE     // Regexes

// Types for non-terminals
%nterm <string> literal type ret_type
%type <expression> expr
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

%left T_OR
%left T_AND
%nonassoc T_LE T_LT T_GE T_GT T_EQ T_NE
%left T_PLUS T_MINUS
%left T_MOD T_DIV T_MULT
%precedence T_NOT

// {$$ = $1;} is already provided.

%%

program : top_lvl_seq {
            module = std::unique_ptr<Top_Level_Seq>($1); set_loc(module, @$);
        }
        ;

top_lvl_seq : top_lvl_item { $$ = new Top_Level_Seq; $$->append($1); set_loc($$, @$); }
            | top_lvl_seq top_lvl_item {
                $$ = $1; $$->append($2); set_loc($$, @$);
            }
            ;

top_lvl_item : function ;

function : func_header statement {
             $$ = new Function{std::move(*$1), $2}; delete $1; set_loc($$, @$);
         }
         | func_header T_ASSIGN expr {
            $$ = new Function{std::move(*$1), new Return_Statement{$3}}; delete $1; set_loc($$, @$);
         }
         ;

func_header : ret_type func_sig { $$ = $2; $$->set_ret_type(std::move(*$1)); delete $1; set_loc($$, @$); }
            | func_sig ret_type { $$ = $1; $$->set_ret_type(std::move(*$2)); delete $2; set_loc($$, @$); }
            ;

ret_type : type | T_PROC { $$ = new std::string{"proc"}; } ;

func_sig : T_IDENT param_group { $$ = new Func_Header{std::move(*$1), std::move(*$2)}; delete $1; delete $2; set_loc($$, @$); } ;

param_group : T_LPAREN T_RPAREN { $$ = new std::vector<Typed_Var>; }
            | T_LPAREN param_list T_RPAREN { $$ = $2; }
            ;

param_list : typed_var { $$ = new std::vector<Typed_Var>{std::move(*$1)}; delete $1; }
           | param_list T_COMMA typed_var { $$ = $1; $$->push_back(std::move(*$3)); delete $3; }
           ;

typed_var : type T_IDENT         { $$ = new Typed_Var(std::move(*$2), std::move(*$1)); delete $1; delete $2; set_loc($$, @$); }
          | T_IDENT T_IS type     { $$ = new Typed_Var(std::move(*$1), std::move(*$3)); delete $1; delete $3; set_loc($$, @$); }
          ;

statement : statement_block { $$ = dynamic_cast<Statement *>($1); } | action T_SEMI | conditional ;

statement_block : T_LBRACE statement_seq T_RBRACE { $$ = $2; set_loc($$, @$); } ;

statement_seq : %empty { $$ = new Statement_Seq; set_loc($$, @$); }
              | statement_seq statement { $$ = $1; $$->append($2); set_loc($$, @$); }
              ;

action : T_RET expr { $$ = new Return_Statement($2); set_loc($$, @$);}
       | T_RET { $$ = new Return_Statement; set_loc($$, @$);}
       | func_call { $$ = dynamic_cast<Statement *>($1); set_loc($$, @$);}
       | T_LET T_IDENT initialization { $$ = new Let_Statement(std::move(*$2), $3); delete $2; set_loc($$, @$); }
       | T_LET typed_var initialization { $$ = new Let_Statement(std::move(*$2), $3); delete $2; set_loc($$, @$); }
       ;

conditional : T_IF expr statement_block else_block { $$ = new If_Statement($2, $3, $4); set_loc($$, @$); }
            | T_IF expr statement    { $$ = new If_Statement($2, $3, nullptr); set_loc($$, @$); }
            ;

else_block : T_ELSE statement_block { $$ = $2; set_loc($$, @$);  }
           | T_ELSE conditional { $$ = $2; set_loc($$, @$);  }
           ;

initialization : T_ASSIGN expr { $$ = $2;  set_loc($$, @$); }
               | T_LBRACE expr T_RBRACE { $$ = $2; set_loc($$, @$);  }
               ;

literal : T_INT | T_FLOAT | T_CHAR | T_BOOL | T_STRING ;

expr  : T_IDENT { $$ = new UserValue(std::move(*$1)); delete $1; set_loc($$, @$);  }
      | func_call { $$ = dynamic_cast<Expression*>($1); set_loc($$, @$);  }
      | literal { $$ = new UserValue(std::move(*$1)); delete $1; set_loc($$, @$);  }
      | T_LPAREN expr T_RPAREN { $$ = $2; set_loc($$, @$);  }
      | T_NOT expr { $$ = new UnaryExpression($1, $2); set_loc($$, @$);  }
      | T_MINUS expr { $$ = new UnaryExpression($1, $2); set_loc($$, @$); } %prec T_NOT
      | expr T_MULT expr { $$ = new BinaryExpression($1, $2, $3); set_loc($$, @$);  }
      | expr T_MOD  expr { $$ = new BinaryExpression($1, $2, $3); set_loc($$, @$);  }
      | expr T_DIV  expr { $$ = new BinaryExpression($1, $2, $3); set_loc($$, @$);  }
      | expr T_PLUS  expr { $$ = new BinaryExpression($1, $2, $3); set_loc($$, @$);  }
      | expr T_MINUS expr { $$ = new BinaryExpression($1, $2, $3); set_loc($$, @$);  }
      | expr T_LE expr { $$ = new BinaryExpression($1, $2, $3); set_loc($$, @$);  }
      | expr T_LT expr { $$ = new BinaryExpression($1, $2, $3); set_loc($$, @$);  }
      | expr T_GE expr { $$ = new BinaryExpression($1, $2, $3); set_loc($$, @$);  }
      | expr T_GT expr { $$ = new BinaryExpression($1, $2, $3); set_loc($$, @$);  }
      | expr T_EQ expr { $$ = new BinaryExpression($1, $2, $3); set_loc($$, @$);  }
      | expr T_NE expr { $$ = new BinaryExpression($1, $2, $3); set_loc($$, @$);  }
      | expr T_AND expr { $$ = new BinaryExpression($1, $2, $3); set_loc($$, @$);  }
      | expr T_OR expr { $$ = new BinaryExpression($1, $2, $3); set_loc($$, @$);  }
      ;

func_call : T_IDENT arg_group {
            $$ = new FunctionCall(std::move(*$1), std::move(*$2)); delete $1; delete $2; set_loc($$, @$);
          }
          | T_IDENT T_DOT func_call {
            std::vector<std::unique_ptr<Expression>> args;
            args.emplace_back($3);
            $$ = new FunctionCall{std::move(*$1), std::move(args)}; delete $1; set_loc($$, @$);
          }
          ;

arg_group : T_LPAREN arg_list T_RPAREN { $$ = $2; }
          | T_LPAREN T_RPAREN { $$ = new std::vector<std::unique_ptr<Expression>>; }
          ;

arg_list : expr {
            $$ = new std::vector<std::unique_ptr<Expression>>;
            $$->emplace_back($1);
         }
         | arg_list T_COMMA expr { $$ = $1; $$->emplace_back($3); }
         ;

type : T_PRIM_TYPE | T_IDENT ;

%%
