%{
%}

// Token definitions

%token T_EQ "==" T_NE "!=" T_LT "<" T_GT ">" T_LE "<=" T_GE  ">="             // Comparisons
%token T_LPAREN "(" T_RPAREN ")" T_LBRACE "{" T_RBRACE "}" T_LBRACK T_RBRACK  // Paired symbols
%token T_PLUS "+" T_MINUS "-" T_DIV "/" T_MULT "*" T_MOD "%"                  // Math symbols
%token T_AMP "&" T_QUESTION "?" T_NULL "null"
%token T_COMMA "," T_IS "is" T_SEMI ";" T_DOT "."                             // Misc symbols
%token T_RET "return" T_IF "if" T_ELSE "else" T_LET "let" T_CONST "const"     // Reserved words
%token T_AND "and" T_OR "or" T_NOT "not"                                      // Boolean operators
%token T_ASSIGN "=" T_ARROW "->"
%token T_FROM "from" T_IMPORT "import" T_EXPORT "export"                      // Reserved words for imports and exports
%token T_IDENT T_INT T_CHAR T_BOOL T_STRING T_FLOAT T_PRIM_TYPE               // Regexes
%token T_THEN "then"

%start program

%precedence if_expr
%left T_OR
%left T_AND
%nonassoc T_LE T_LT T_GE T_GT T_EQ T_NE
%left T_PLUS T_MINUS
%left T_MOD T_DIV T_MULT
%precedence T_NOT
%precedence T_DOT

%%

program : imports top_lvl_seq
        ;

imports : %empty
        | imports T_FROM T_STRING T_IMPORT import_list
        ;

import_list : T_IDENT
            | import_list T_COMMA T_IDENT
            ;

top_lvl_seq : top_lvl_item
            | top_lvl_seq top_lvl_item
            ;

top_lvl_item : internal_decl
             | export_decl
             ;

export_decl : T_EXPORT "{" internal_decl_seq "}"
            | T_EXPORT internal_decl
            ;

internal_decl_seq : internal_decl
                  | internal_decl_seq internal_decl
                  ;

internal_decl : const_decl
              | function
              | struct_definition
              ;

const_decl : T_CONST typed_var T_ASSIGN expr
           ;

function : func_header stmt
         | func_header T_ASSIGN expr
         ;

struct_definition : T_IDENT struct_body
                  ;

struct_body : T_LBRACE struct_fields T_RBRACE
            ;

struct_fields : struct_field
              | struct_fields T_COMMA
              | struct_fields T_SEMI
              | struct_fields struct_field
              ;

struct_field : typed_var
             | typed_var "=" expr
             ;

func_header : func_sig ret_type
            ;

ret_type : T_ARROW type
         | %empty
         ;

func_sig : T_IDENT param_group
         ;

param_group : T_LPAREN T_RPAREN
            | T_LPAREN param_list T_RPAREN
            ;

param_list : typed_var
           | param_list T_COMMA typed_var
           ;

typed_var : type T_IDENT
          | T_IDENT T_IS type
          ;

stmt : stmt_block
     | return_stmt T_SEMI
     | let_stmt T_SEMI
     | conditional
     | func_call opt_semi
     ;

opt_semi : %empty
         | T_SEMI
         ;

stmt_block : T_LBRACE stmt_seq T_RBRACE
           ;

stmt_seq : %empty
         | stmt_seq stmt
         ;

let_stmt : T_LET T_IDENT T_ASSIGN expr
         | T_LET typed_var T_ASSIGN expr
         ;

return_stmt : T_RET expr
            | T_RET
            ;

conditional : if_head stmt_block T_ELSE stmt
            | if_head stmt
            ;

if_head : T_IF T_LPAREN expr T_RPAREN
        | T_IF expr T_THEN
        ;

atom : literal
     | T_IDENT
     | func_call
     | struct_init
     | T_LPAREN expr T_RPAREN
     ;

struct_init : T_IDENT T_LBRACE struct_field_init T_RBRACE
            ;

struct_field_init : T_IDENT "=" expr
                  | struct_field_init T_COMMA
                  | struct_field_init T_SEMI
                  | struct_field_init T_IDENT "=" expr
                  ;

func_call : T_IDENT arg_group
          ;

literal : T_INT
        | T_FLOAT
        | T_CHAR
        | T_NULL
        | T_BOOL
        | T_STRING
        ;

expr : atom
     | T_IF expr T_THEN expr T_ELSE expr %prec if_expr
     | T_NOT expr
     | T_MULT expr   %prec T_NOT
     | T_MINUS expr  %prec T_NOT
     | expr T_DOT T_IDENT
     | expr T_MULT expr
     | expr T_MOD expr
     | expr T_DIV expr
     | expr T_PLUS expr
     | expr T_MINUS expr
     | expr T_LE expr
     | expr T_LT expr
     | expr T_GE expr
     | expr T_GT expr
     | expr T_EQ expr
     | expr T_NE expr
     | expr T_AND expr
     | expr T_OR expr
     ;

arg_group : T_LPAREN arg_list T_RPAREN
          | T_LPAREN T_RPAREN
          ;

arg_list : expr
         | arg_list T_COMMA expr
         ;

type : T_PRIM_TYPE
     | T_IDENT
     | T_QUESTION type
     | T_AMP type
     ;

%%
