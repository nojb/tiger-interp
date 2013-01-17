type value =
  | Vunit
  | Vint of int
  | Vstring of string
  | Varray of value array
  | Vrecord of value array option

type comparison =
  | Ceq | Cle | Clt | Cne | Cge | Cgt

type expr =
  | Cquote of value
  | Cget of int * int
  | Cset of int * int * expr
  | Cload of expr * expr
  | Cstore of expr * expr * expr
  | Cgetf of expr * int
  | Csetf of expr * int * expr
  | Cadd of expr * expr
  | Cmul of expr * expr
  | Cdiv of expr * expr
  | Csub of expr * expr
  | Cicmp of expr * comparison * expr
  | Cscmp of expr * comparison * expr
  | Candalso of expr * expr
  | Corelse of expr * expr
  | Ccall of proc * expr array
  | Cseq of expr array
  | Cmakearray of expr * expr
  | Cmakerecord of expr array
  | Cif of expr * expr * expr
  | Cwhile of expr * expr
  | Cfor of int * int * expr * expr * expr
  | Cbreak
  | Cprim of prim * expr array

and proc = {
  mutable proc_body : expr;
  mutable proc_frame_size : int;
  proc_depth : int
}

and prim =
  value array -> value

exception Break
exception Nil
exception Exit of int

let rec string_of_value = function (* loops if circular data type *)
  | Vunit -> ""
  | Vint n -> string_of_int n
  | Vstring s -> s
  | Varray a ->
      "[" ^ (String.concat "; " (Array.to_list (Array.map string_of_value a))) ^ "]"
  | Vrecord None ->
      "nil"
  | Vrecord (Some a) ->
      "{" ^ (String.concat "; " (Array.to_list (Array.map string_of_value a))) ^ "}"
