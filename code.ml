type value =
  | Vunit
  | Vint of int
  | Vstring of string
  | Varray of value array
  | Vrecord of value array option

type comparison =
  | Ceq | Cle | Clt | Cne | Cge | Cgt

type primitive =
  | Pprint
  | Pprinti
  | Pflush
  | Pgetchar
  | Pord
  | Pchr
  | Psize
  | Psubstring
  | Pconcat
  | Pnot
  | Pexit

type code =
  | Cquote of value
  | Cget of int * int
  | Cset of int * int * code
  | Cload of int * code * code
  | Cstore of int * code * code * code
  | Cgetf of int * code * int
  | Csetf of int * code * int * code
  | Cadd of code * code
  | Cmul of code * code
  | Cdiv of int * code * code
  | Csub of code * code
  | Cicmp of code * comparison * code
  | Cscmp of code * comparison * code
  | Ccall of proc * code array
  | Cseq of code * code
  | Cmakearray of code * code
  | Cmakerecord of code array
  | Cif of code * code * code
  | Cwhile of code * code
  | Cfor of int * int * code * code * code
  | Cbreak
  | Cprim of primitive * code array

and proc = {
  mutable proc_code : code;
  mutable proc_frame_size : int;
  proc_depth : int;
  proc_name : string
}

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
