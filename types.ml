type tiger_type =
  | TIGvoid
  | TIGint
  | TIGstring
  | TIGarray of tiger_type * int
  | TIGrecord of (string * tiger_type) list * int
  | TIGforward of string
  | TIGanyarray

module StringMap = Map.Make(String)

let rec unroll env t =
  match t with
  | TIGforward id -> unroll env (StringMap.find id env)
  | _ -> t

let rec type_equal t1 t2 =
  match t1, t2 with
  | TIGvoid, TIGvoid
  | TIGint, TIGint
  | TIGstring, TIGstring -> true
  | TIGarray (_, id), TIGarray (_, id')
  | TIGrecord (_, id), TIGrecord (_, id') -> id = id'
  | TIGanyarray, TIGarray _
  | TIGarray _, TIGanyarray -> true
  | TIGforward _, _
  | _, TIGforward _ -> failwith "type_equal"
  | _ -> false

let type_count = ref 0

let new_array_type t =
  incr type_count;
  TIGarray (t, !type_count)

let new_record_type ts =
  incr type_count;
  TIGrecord (ts, !type_count)

let base_tenv =
  StringMap.add "int" TIGint
    (StringMap.add "string" TIGstring StringMap.empty)
