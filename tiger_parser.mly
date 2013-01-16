%{
  open Tiger_ast
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

%type <Tiger_ast.exp Tiger_ast.loc> program
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
    e=loc(exp) EOF
  { e }
  ;

expseq:
    xs = separated_list(SEMI, loc(exp))
  { xs }
  ;

fields:
    xs = separated_list(COMMA, separated_pair(loc(IDENT), EQ, loc(exp)))
  { xs }
  ;

%inline bin:
    LAND    { Op_and }
  | LOR     { Op_or }
  | PLUS    { Op_add }
  | TIMES   { Op_mul }
  | MINUS   { Op_sub }
  | SLASH   { Op_div }
  | EQ      { Op_eq }
  | NE      { Op_ne }
  | LE      { Op_leq }
  | LT      { Op_lt }
  | GE      { Op_geq }
  | GT      { Op_gt }
  ;

exp:
    INT
  { Eint $1 }
  | STRING
  { Estring $1 }
  | NIL
  { Enil }
  | v=var
  { v }
  | MINUS x=loc(exp) %prec unary_op
  { Ebinop ((Eint 0, Tiger_loc.make $startpos ($startpos(x))), Op_sub, x) }
  | x=loc(exp) op=bin y=loc(exp)
  { Ebinop (x, op, y) }
  | v=loc(var) COLONEQ e=loc(exp)
  { let (v, loc) = v in
    match v with
    | Esimple x -> Eassign (x, e)
    | Eload (v, x) -> Estore (v, x, e)
    | Eget (v, x) -> Eput (v, x, e)
    | _ -> assert false }
  | x=loc(IDENT) LPAREN xs=separated_list(COMMA, loc(exp)) RPAREN
  { Ecall (x, xs) }
  | LPAREN expseq RPAREN
  { Eseq $2 }
  | x=loc(IDENT) LCURLY xs=fields RCURLY
  { Erecord (x, xs) }
  | i=loc(IDENT) LBRACK x=loc(exp) RBRACK OF y=loc(exp)
  { Earray (i, x, y) }
  | IF x=loc(exp) THEN y=loc(exp)
  { Eif (x, y, None) }
  | IF x=loc(exp) THEN y=loc(exp) ELSE z=loc(exp)
  { Eif (x, y, Some z) }
  | WHILE x=loc(exp) DO y=loc(exp)
  { Ewhile (x, y) }
  | FOR i=IDENT COLONEQ x=loc(exp) TO y=loc(exp) DO z=loc(exp)
  { Efor (i, x, y, z) }
  | BREAK
  { Ebreak }
  | LET e=letexp
  { e }
  ;

%inline loc(X):
    x=X     { (x, Tiger_loc.make $startpos $endpos) }
  ;

var:
    i=loc(IDENT)  { Esimple i }
  | longvar       { $1 }
  ;

longvar:
    i=loc(IDENT) LBRACK e=loc(exp) RBRACK
  { Eload ((Esimple i, Tiger_loc.make ($startpos(i)) ($endpos(i))), e) }
  | v=loc(var) DOT i=loc(IDENT)
  { Eget (v, i) }
  ;

letexp:
    v=vardec ee=loc(letexp2)
  { let (x, y, e) = v in Eletvar (x, y, e, ee) }
  | ts=typdecs e=loc(letexp2)
  { Elettype (ts, e) }
  | fs=fundecs e=loc(letexp2)
  { Eletfuns (fs, e) }
  ;

letexp2:
    letexp              { $1 }
  | IN ee = expseq END  { Eseq ee }
  ;

vardec:
    VAR x=IDENT y = option(preceded(COLON, loc(IDENT))) COLONEQ e=loc(exp)
  { (x, y, e) }
  ;

typdecs:
    xs=nonempty_list(preceded(TYPE, separated_pair(IDENT, EQ, typ)))
  { xs }
  ;

typfields:
    xs=separated_list(COMMA, separated_pair(loc(IDENT), COLON, loc(IDENT)))
  { xs }
  ;

typ:
    i=loc(IDENT)                { Tname i }
  | ARRAY OF i=loc(IDENT)       { Tarray i }
  | LCURLY xs=typfields RCURLY  { Trecord xs } 
  ;

fundecs:
    xs=nonempty_list(fundec)
  { xs }
  ;

fundec:
  FUNCTION x=loc(IDENT) LPAREN xs=typfields RPAREN
    y = option(preceded(COLON, loc(IDENT))) EQ z=loc(exp)
  {
    { fun_name = x; fun_args = xs;
      fun_rety = y; fun_body = z }
  }
  ;
