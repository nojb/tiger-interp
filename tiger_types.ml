type tiger_type =
  | TIGvoid
  | TIGint
  | TIGstring
  | TIGarray of tiger_type * int
  | TIGrecord of (string * tiger_type) array * int
  | TIGforward of string

let rec type_equal t1 t2 =
  match t1, t2 with
  | TIGvoid, TIGvoid
  | TIGint, TIGint
  | TIGstring, TIGstring -> true
  | TIGarray (_, id), TIGarray (_, id')
  | TIGrecord (_, id), TIGrecord (_, id') -> id = id'
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

module M = Map.Make (String)

let base_tenv =
  M.add "int" TIGint
    (M.add "string" TIGstring M.empty)
