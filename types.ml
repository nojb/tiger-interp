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

let rec describe_type = function
  | TIGvoid -> "void"
  | TIGint -> "int"
  | TIGstring -> "string"
  | TIGarray(id, t) -> Printf.sprintf "%s = array of %s" id (describe_type t)
  | TIGrecord(id, xts) ->
      Printf.sprintf "%s = {%s}" id
        (String.concat ", "
          (List.map (fun (x, t) -> x ^ " : " ^ describe_type t) xts))
  | TIGnamed id -> id

let base_tenv =
  StringMap.add "int" TIGint
    (StringMap.add "string" TIGstring StringMap.empty)
