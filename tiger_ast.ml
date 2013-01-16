type 'a loc =
  'a * Tiger_loc.t

type bin =
  | Op_add | Op_sub | Op_mul | Op_div
  | Op_eq | Op_leq | Op_geq | Op_ne
  | Op_gt | Op_lt | Op_and | Op_or

type exp =
  | Eint of int
  | Estring of string
  | Enil
  | Esimple of string loc
  | Eload of exp loc * exp loc
  | Eget of exp loc * string loc
  | Ebinop of exp loc * bin * exp loc
  | Eassign of string loc * exp loc
  | Estore of exp loc * exp loc * exp loc
  | Eput of exp loc * string loc * exp loc
  | Ecall of string loc * exp loc list
  | Eseq of exp loc list
  | Earray of string loc * exp loc * exp loc
  | Erecord of string loc * (string loc * exp loc) list
  | Eif of exp loc * exp loc * exp loc option
  | Ewhile of exp loc * exp loc
  | Efor of string * exp loc * exp loc * exp loc
  | Ebreak
  | Eletvar of string * string loc option * exp loc * exp loc
  | Eletfuns of fundec list * exp loc
  | Elettype of (string * typ) list * exp loc

and fundec = {
  fun_name : string loc;
  fun_args : (string loc * string loc) list;
  fun_rety : string loc option;
  fun_body : exp loc
}

and typ =
  | Tname of string loc
  | Tarray of string loc
  | Trecord of (string loc * string loc) list
