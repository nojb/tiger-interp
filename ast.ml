type pos =
  Lexing.position

type ident = {
  id : string;
  pos : pos
}

type bin =
  | Op_add | Op_sub | Op_mul | Op_div
  | Op_eq | Op_leq | Op_geq | Op_ne
  | Op_gt | Op_lt | Op_and | Op_or

type typ =
  | Tname     of ident
  | Tarray    of ident
  | Trecord   of (ident * ident) list

type var =
  | Vsimple     of ident
  | Vsubscript  of pos * var * exp
  | Vfield      of pos * var * ident

and exp =
  | Eunit     of pos
  | Eint      of pos * int
  | Estring   of pos * string
  | Enil      of pos
  | Evar      of var
  | Ebinop    of pos * exp * bin * exp
  | Eassign   of pos * var * exp
  | Ecall     of pos * ident * exp list
  | Eseq      of exp * exp
  | Earray    of pos * ident * exp * exp
  | Erecord   of pos * ident * (ident * exp) list
  | Eif       of pos * exp * exp * exp
  | Ewhile    of pos * exp * exp
  | Efor      of pos * ident * exp * exp * exp
  | Ebreak    of pos
  | Elet      of pos * dec * exp

and dec =
  | Dtypes  of (ident * typ) list
  | Dfuns   of fundec list
  | Dvar    of ident * ident option * exp

and fundec = {
  fun_name : ident;
  fun_args : (ident * ident) list;
  fun_rety : ident option;
  fun_body : exp
}

let rec var_pos = function
  | Vsimple id -> id.pos
  | Vsubscript(p, _, _) | Vfield(p, _, _) -> p

and exp_pos = function
  | Eunit p | Eint(p, _) | Estring(p, _) | Enil p -> p
  | Evar v -> var_pos v
  | Ebinop(p, _, _, _) | Eassign(p, _, _) | Ecall(p, _, _) -> p
  | Eseq(e, _) -> exp_pos e
  | Earray(p, _, _, _) | Erecord(p, _, _)
  | Eif(p, _, _, _) | Ewhile(p, _, _) | Efor(p, _, _, _, _) -> p
  | Ebreak p | Elet(p, _, _) -> p
