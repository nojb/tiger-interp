%{
  open Error
  open Ast

  let pos i =
    Parsing.rhs_start_pos i
%}

%token ARRAY OF
%token NIL BREAK
%token COLON COMMA
%token VAR FUNCTION TYPE
%token COLONEQ
%token LAND LOR
%token EQ NE LE LT GE GT
%token FOR WHILE TO DO
%token PLUS MINUS TIMES SLASH
%token SEMI
%token DOT
%token LET IN END
%token IF THEN ELSE
%token LCURLY RCURLY
%token LBRACK RBRACK
%token LPAREN RPAREN
%token <int> INT
%token <string> IDENT
%token <string> STRING
%token EOF

%type <Ast.exp> program
%start program

%left THEN
%left ELSE
%nonassoc COLONEQ
%left OF DO
%left LOR
%left LAND
%nonassoc LE GE EQ NE GT LT
%left PLUS MINUS
%left TIMES SLASH
%right unary_op

%%

program:
  exp EOF { $1 }
| error { raise (Error(Parsing.symbol_start_pos (), "syntax error")) }
;

ident:
  IDENT
  { { id = $1; pos = pos 1 } }
;

expseq:
  /* empty */
  { Eunit (Parsing.symbol_start_pos ()) }
| expseq_tail
  { $1 }
;

expseq_tail:
  exp
  { $1 }
| exp SEMI expseq_tail
  { Eseq($1, $3) }
;

fields:
    ident EQ exp
  { [($1, $3)] }
  | ident EQ exp COMMA fields
  { ($1, $3) :: $5 }
  ;

exp:
  INT         { Eint(pos 1, $1) }
| STRING      { Estring(pos 1, $1) }
| NIL         { Enil(pos 1) }
| var         { Evar $1 }
| MINUS exp %prec unary_op              { Ebinop(pos 1, Eint(pos 1, 0), Op_sub, $2) }
| exp LAND exp { Eif(pos 1, $1, $3, Eint(pos 3, 0)) }
| exp LOR exp { Eif(pos 1, $1, Eint(pos 1, 1), $3) }
| exp PLUS exp { Ebinop(pos 2, $1, Op_add, $3) }
| exp MINUS exp { Ebinop(pos 2, $1, Op_sub, $3) }
| exp TIMES exp { Ebinop(pos 2, $1, Op_mul, $3) }
| exp SLASH exp { Ebinop(pos 2, $1, Op_div, $3) }
| exp EQ exp { Ebinop(pos 2, $1, Op_eq, $3) }
| exp NE exp { Ebinop(pos 2, $1, Op_ne, $3) }
| exp LE exp { Ebinop(pos 2, $1, Op_leq, $3) }
| exp LT exp { Ebinop(pos 2, $1, Op_lt, $3) }
| exp GE exp { Ebinop(pos 2, $1, Op_geq, $3) }
| exp GT exp { Ebinop(pos 2, $1, Op_gt, $3) }
| var COLONEQ exp                       { Eassign(pos 2, $1, $3) }
| ident LPAREN exp_list RPAREN          { Ecall(pos 1, $1, $3) }
| LPAREN expseq RPAREN                  { $2 }
| ident LCURLY fields RCURLY            { Erecord(pos 2, $1, $3) }
| var LBRACK exp RBRACK OF exp
    { match $1 with
      | Vsimple(x) -> Earray(pos 2, x, $3, $6)
      | _ -> raise Parse_error }
| IF exp THEN exp                       { Eif(pos 1, $2, $4, Eunit(pos 4)) }
| IF exp THEN exp ELSE exp              { Eif(pos 1, $2, $4, $6) }
| WHILE exp DO exp                      { Ewhile(pos 1, $2, $4) }
| FOR ident COLONEQ exp TO exp DO exp   { Efor(pos 1, $2, $4, $6, $8) }
| BREAK                                 { Ebreak(pos 1) }
| LET decs IN expseq END { List.fold_right (fun (p, d) e -> Elet(p, d, e)) $2 $4 }
;

exp_list:
  /* empty */
  { [] }
  | exp_list_tail
  { $1 }
  ;

exp_list_tail:
  exp
  { [$1] }
| exp COMMA exp_list_tail
{ $1 :: $3 }
;

var:
    ident
  { Vsimple $1 }
  | var LBRACK exp RBRACK
  { Vsubscript(pos 2, $1, $3) }
  | var DOT ident
  { Vfield(pos 2, $1, $3) }
  ;

decs:
    vardec decs_vtf
  { (pos 1, $1) :: $2 }
  | typdecs decs_vf
  { (pos 1, Dtypes $1) :: $2 }
  | fundec_list decs_vt
  { (pos 1, Dfuns $1) :: $2 }
  ;

decs_vtf:
    /* empty */
  { [] }
  | vardec decs_vtf
  { (pos 1, $1) :: $2 }
  | typdecs decs_vf
  { (pos 1, Dtypes $1) :: $2 }
  | fundec_list decs_vt
  { (pos 1, Dfuns $1) :: $2 }
  ;

decs_vf:
    /* empty */
  { [] }
  | vardec decs_vtf
  { (pos 1, $1) :: $2 }
  | fundec_list decs_vt
  { (pos 1, Dfuns $1) :: $2 }
  ;

decs_vt:
    /* empty */
  { [] }
  | vardec decs_vtf
  { (pos 1, $1) :: $2 }
  | typdecs decs_vf
  { (pos 1, Dtypes $1) :: $2 }
  ;

vardec:
  VAR ident optional_var_type COLONEQ exp
  { Dvar($2, $3, $5) }
  ;

optional_var_type:
  /* empty */
  { None }
  | COLON ident
  { Some $2 }
  ;

typdecs:
  TYPE ident EQ typ
  { [($2, $4)] }
  | TYPE ident EQ typ typdecs
  { ($2, $4) :: $5 }
  ;

type_field_list:
  /* empty */ { [] }
| type_field_list_tail { $1 }
;

type_field_list_tail:
  ident COLON ident { [($1, $3)] }
| ident COLON ident COMMA type_field_list_tail { ($1, $3) :: $5 }
  ;

typ:
    ident                { Tname $1 }
  | ARRAY OF ident       { Tarray $3 }
  | LCURLY type_field_list RCURLY  { Trecord $2 } 
  ;

fundec_list:
    fundec
    { [$1] }
| fundec fundec_list
  { $1 :: $2 }
  ;

fundec:
  FUNCTION ident LPAREN type_field_list RPAREN
    optional_var_type EQ exp
  {
    { fun_name = $2; fun_args = $4;
      fun_rety = $6; fun_body = $8 }
  }
  ;
