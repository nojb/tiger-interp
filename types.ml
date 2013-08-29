type tiger_type =
  | TIGvoid
  | TIGint
  | TIGstring
  | TIGarray of string * tiger_type
  | TIGrecord of string * (string * tiger_type) list
  | TIGnamed of string

module StringMap = Map.Make(String)

let rec unroll tenv t =
  match t with
  | TIGnamed id -> unroll tenv (StringMap.find id tenv)
  | _ -> t

let rec type_equal tenv t1 t2 =
  unroll tenv t1 == unroll tenv t2

let base_tenv =
  StringMap.add "int" TIGint
    (StringMap.add "string" TIGstring StringMap.empty)
