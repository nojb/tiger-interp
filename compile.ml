open Types
open Code
open Ast

type error =
  | Unbound_variable of string
  | Unbound_function of string
  | Unbound_type of string
  | Expected_array_type
  | Expected_record_type
  | Unknown_field of string
  | Expected_array
  | Expected_record
  | Type_mismatch of tiger_type * tiger_type
  | Bad_arity of int * int
  | Illegal_nil
  | Illegal_break
  | Expected_record_array_elements
  | Expected_record_field
  | Unexpected_field of string
  | Not_enough_fields
  | Too_many_fields
  | Bad_cyclic_types

exception Error of Lexing.position * error

module M = Map.Make (String)

let find_variable x venv =
  try
    let vd = Env.find_variable venv x.id in
    vd.Env.var_type, vd.Env.var_depth, vd.Env.var_frame_index
  with
    Not_found -> raise (Error(x.pos, Unbound_variable x.id))

let find_function x venv =
  try
    Env.find_function venv x.id
  with
    Not_found -> raise (Error(x.pos, Unbound_function x.id))
  
let find_type x tenv =
  try
    M.find x.id tenv
  with
    Not_found -> raise (Error (x.pos, Unbound_type x.id))

let rec check_type tenv (x, t) =
  let visited = ref [] in
  let rec loop thru_record t =
    if List.memq t !visited then
      if thru_record then ()
      else raise (Error(x.pos, Bad_cyclic_types))
    else begin
      visited := t :: !visited;
      match t with
      | TIGvoid | TIGint | TIGstring -> ()
      | TIGarray (_, t) ->
          loop thru_record t
      | TIGrecord (_, xts) ->
          List.iter (fun (_, t) -> loop true t) xts
      | TIGnamed y ->
          begin try
            loop thru_record (M.find y tenv)
          with
            Not_found -> raise (Error(x.pos, Unbound_type y))
            (* FIXME x.p != position of y in general *)
          end
    end
  in loop false (M.find x.id tenv)

let find_array_type x tenv =
  match unroll tenv (find_type x tenv) with
  | TIGarray (_, t') as t -> t, t'
  | _ -> raise (Error (x.pos, Expected_array_type))

let find_record_type x tenv =
  match unroll tenv (find_type x tenv) with
  | TIGrecord (_, ts) as t -> t, ts
  | _ -> raise (Error (x.pos, Expected_record_type))

let find_record_field (x, loc) ts =
  let rec loop i =
    if i >= Array.length ts then
      raise (Error (loc, Unknown_field x))
    else if fst ts.(i) = x then i, snd ts.(i)
    else loop (i+1)
  in loop 0

(** Translation of variables *)

let rec array_var tenv venv inloop v =
  let v', t = type_var tenv venv inloop v in
  match unroll tenv t with
  | TIGarray(_, t') -> v', t'
  | _ -> raise (Error(var_pos v, Expected_array))

and record_var tenv venv inloop v x =
  let v', t = type_var tenv venv inloop v in
  match unroll tenv t with
  | TIGrecord(_, ts) ->
      let rec loop i = function
        | [] -> raise (Error(x.pos, Unexpected_field x.id))
        | (x', t) :: xts when x.id = x' -> v', i, t
        | _ :: xts -> loop (i+1) xts
      in loop 0 ts
  | _ -> raise (Error(var_pos v, Expected_record))

(** Translation of expressions *)

and exp_with_type tenv venv inloop (e : exp) t' : Code.code =
  let e', t = type_exp tenv venv inloop e in
  if type_equal tenv t t' then e'
  else raise (Error(exp_pos e, Type_mismatch(unroll tenv t, unroll tenv t')))

and int_exp tenv venv inloop e =
  exp_with_type tenv venv inloop e TIGint

and void_exp tenv venv inloop e =
  exp_with_type tenv venv inloop e TIGvoid

and transl_call tenv venv inloop x xs =
  let func = Env.find_function venv x.id in
  let (ts, t) = func.Env.fn_signature in

  if List.length xs <> List.length ts then
    raise (Error(x.pos, Bad_arity(List.length ts, List.length xs)));

  let rec loop args xs ts =
    match xs, ts with
    | [], [] ->
        let args = Array.of_list (List.rev args) in
        begin match func.Env.fn_desc with
        | Env.User f -> Ccall (f, args)
        | Env.Builtin p -> Cprim (p, args) end
    | x :: xs, t :: ts ->
        let x = exp_with_type tenv venv inloop x t in
        loop (x :: args) xs ts
    | _ -> assert false in
  (loop [] xs ts), t

and type_var tenv venv inloop v =
  match v with
  | Vsimple id ->
      let t, d, i = find_variable id venv in
      Cget(d, i), t
  | Vsubscript(p, v, e) ->
      let v, t = array_var tenv venv inloop v in
      let x = int_exp tenv venv inloop e in
      Cload(p.Lexing.pos_lnum, v, x), t
  | Vfield(p, v, id) ->
      let v, i, t = record_var tenv venv inloop v id in
      Cgetf(p.Lexing.pos_lnum, v, i), t

and type_exp tenv venv inloop (e : exp) : Code.code * Types.tiger_type =
  match e with
  | Eunit _ ->
      Cquote Vunit, TIGvoid
  | Eint(_, n) ->
      Cquote(Vint n), TIGint
  | Estring(_, s) ->
      Cquote(Vstring s), TIGstring
  | Enil p ->
      raise (Error(p, Illegal_nil))
  | Evar v ->
      type_var tenv venv inloop v
  | Ebinop(_, x, Op_add, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = int_exp tenv venv inloop y in
      Cadd(e1, e2), TIGint
  | Ebinop(_, x, Op_sub, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = int_exp tenv venv inloop y in
      Csub(e1, e2), TIGint
  | Ebinop(_, x, Op_mul, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = int_exp tenv venv inloop y in
      Cmul(e1, e2), TIGint
  | Ebinop(p, x, Op_div, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = int_exp tenv venv inloop y in
      Cdiv(p.Lexing.pos_lnum, e1, e2), TIGint
  | Ebinop(_, x, Op_eq, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = int_exp tenv venv inloop y in
      Cicmp(e1, Ceq, e2), TIGint
  | Ebinop(_, x, Op_ne, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = int_exp tenv venv inloop y in
      Cicmp(e1, Cne, e2), TIGint
  | Ebinop(_, x, Op_leq, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = int_exp tenv venv inloop y in
      Cicmp(e1, Cle, e2), TIGint
  | Ebinop(_, x, Op_lt, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = int_exp tenv venv inloop y in
      Cicmp(e1, Clt, e2), TIGint
  | Ebinop(_, x, Op_geq, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = int_exp tenv venv inloop y in
      Cicmp(e1, Cge, e2), TIGint
  | Ebinop(_, x, Op_gt, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = int_exp tenv venv inloop y in
      Cicmp(e1, Cgt, e2), TIGint
  | Eassign(_, Vsimple id, Enil _) ->
      let t, d, i = find_variable id venv in
      begin match unroll tenv t with
      | TIGrecord _ ->
          Cset(d, i, Cquote(Vrecord None)), TIGvoid
      | _ ->
          raise (Error(id.pos, Expected_record))
      end
  | Eassign(_, Vsimple id, e) ->
      let t, d, i = find_variable id venv in
      let e = exp_with_type tenv venv inloop e t in
      Cset(d, i, e), TIGvoid
  | Eassign(p, Vsubscript(_, v, x), Enil _) ->
      let v', t = array_var tenv venv inloop v in
      begin match unroll tenv t with
      | TIGrecord _ ->
          let x = int_exp tenv venv inloop x in
          Cstore(p.Lexing.pos_lnum, v', x, Cquote(Vrecord None)), TIGvoid
      | _ ->
          raise (Error(var_pos v, Expected_record_array_elements))
      end
  | Eassign(p, Vsubscript(_, v, x), e) ->
      let v, t = array_var tenv venv inloop v in
      let x = int_exp tenv venv inloop x in
      let e = exp_with_type tenv venv inloop e t in
      Cstore (p.Lexing.pos_lnum, v, x, e), TIGvoid
  | Eassign(p, Vfield(_, v, x), Enil _) ->
      let v', i, t = record_var tenv venv inloop v x in
      begin match unroll tenv t with
      | TIGrecord _ ->
          Csetf(p.Lexing.pos_lnum, v', i, Cquote(Vrecord None)), TIGvoid
      | _ ->
          raise (Error(var_pos v, Expected_record_field))
      end
  | Eassign(p, Vfield(_, v, x), e) ->
      let v, i, t = record_var tenv venv inloop v x in
      let e = exp_with_type tenv venv inloop e t in
      Csetf(p.Lexing.pos_lnum, v, i, e), TIGvoid
  | Ecall(_, x, xs) ->
      (* FIXME loc *)
      transl_call tenv venv inloop x xs
  | Eseq(x, y) ->
      let x, _ = type_exp tenv venv inloop x in
      let y, t = type_exp tenv venv inloop y in
      Cseq(x, y), t
  | Earray(_, x, y, Enil _) ->
      let t, t' = find_array_type x tenv in
      begin match unroll tenv t' with
      | TIGrecord _ ->
          let y = int_exp tenv venv inloop y in
          Cmakearray(y, Cquote(Vrecord(None))), t
      | _ ->
          raise (Error(x.pos, Expected_record_array_elements))
      end
  | Earray(_, x, y, z) ->
      let t, t' = find_array_type x tenv in
      let y = int_exp tenv venv inloop y in
      let z = exp_with_type tenv venv inloop z t' in
      Cmakearray(y, z), t
  | Erecord(p, t, xs) ->
      let t, ts = find_record_type t tenv in
      let rec bind vs = function
        | [], [] ->
            Cmakerecord(Array.of_list (List.rev vs)), t
        | (x, Enil _) :: xts, (x', t) :: ts ->
            if x.id = x' then
              bind (Cquote(Vrecord None) :: vs) (xts, ts)
            else
              raise (Error(x.pos, Unexpected_field x.id))
        | (x, e) :: xts, (x', t) :: ts ->
            if x.id = x' then
              let e = exp_with_type tenv venv inloop e t in
              bind (e :: vs) (xts, ts)
            else
              raise (Error(x.pos, Unexpected_field x.id))
        | [], _ ->
            raise (Error(p, Not_enough_fields))
        | _, [] ->
            raise (Error(p, Too_many_fields))
      in bind [] (xs, ts)
  | Eif(_, x, y, z) ->
      let e1 = int_exp tenv venv inloop x in
      let e2, t2 = type_exp tenv venv inloop y in
      let e3 = exp_with_type tenv venv inloop z t2 in
      Cif(e1, e2, e3), t2
  | Ewhile(_, x, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = void_exp tenv venv true y in
      Cwhile(e1, e2), TIGvoid
  | Efor(_, i, x, y, z) ->
      let x = int_exp tenv venv inloop x in
      let y = int_exp tenv venv inloop y in
      let venv = Env.new_scope venv in
      let d, i = Env.add_variable venv i.id TIGint in
      let z = void_exp tenv venv true z in
      Cfor(d, i, x, y, z), TIGvoid
  | Ebreak p ->
      if inloop then Cbreak, TIGvoid
      else raise (Error(p, Illegal_break))
  | Elet(_, Dvar(x, None, y), z) ->
      let y, t = type_exp tenv venv inloop y in
      let venv = Env.new_scope venv in
      let d, i = Env.add_variable venv x.id t in
      let z, t = type_exp tenv venv inloop z in
      Cseq(Cset (d, i, y), z), t
  | Elet(_, Dvar(x, Some t, Enil _), z) ->
      let t, _ = find_record_type t tenv in
      let venv = Env.new_scope venv in
      let d, i = Env.add_variable venv x.id t in
      let z, t = type_exp tenv venv inloop z in
      Cseq(Cset (d, i, Cquote(Vrecord None)), z), t
  | Elet(_, Dvar(x, Some t, y), z) ->
      let t = find_type t tenv in
      let y = exp_with_type tenv venv inloop y t in
      let venv = Env.new_scope venv in
      let d, i = Env.add_variable venv x.id t in
      let z, t = type_exp tenv venv inloop z in
      Cseq(Cset (d, i, y), z), t
  | Elet(_, Dtypes tys, e) ->
      type_exp (lettype tenv tys) venv inloop e
  | Elet(_, Dfuns funs, e) ->
      let venv = letfuns tenv venv funs in
      type_exp tenv venv inloop e

(** Translation of types *)

and lettype tenv tys =
  let find_type x tenv =
    try M.find x.id tenv
    with Not_found -> TIGnamed x.id
  in
  let deftype tenv (x, t) =
    match t with
    | Tname (y) ->
        M.add x.id (find_type y tenv) tenv
    | Tarray y ->
        M.add x.id (TIGarray(x.id, find_type y tenv)) tenv
    | Trecord xs ->
        let field (x, t) = (x.id, find_type t tenv) in
        M.add x.id (TIGrecord(x.id, List.map field xs)) tenv
  in
  let tenv = List.fold_left deftype tenv tys in
  List.iter (check_type tenv) tys;
  tenv

(** Translation of functions *)

and function_signature tenv fn =
  let ty = match fn.fun_rety with
  | None -> TIGvoid
  | Some t -> find_type t tenv in
  (List.map (fun (_, t) -> find_type t tenv) fn.fun_args, ty)

and function_name fn =
  fn.fun_name.id

and letfuns tenv venv funs =

  let declare_function venv func =
    Env.add_function venv
      (function_name func)
      (function_signature tenv func) in

  let update_function fn venv body =
    match fn.Env.fn_desc with
    | Env.User p ->
        p.proc_code <- body;
        p.proc_frame_size <- Env.frame_size venv
    | _ -> assert false in

  let define_function venv func =
    let fn = Env.find_function venv func.fun_name.id in
    let ts, t = fn.Env.fn_signature in
    let venv = Env.new_frame venv in
    List.iter2 (fun (x, _) t ->
      ignore (Env.add_variable venv x.id t)) func.fun_args ts;
    let body = exp_with_type tenv venv false func.fun_body t in
    update_function fn venv body in

  let venv = Env.new_scope venv in
  List.iter (declare_function venv) funs;
  List.iter (define_function venv) funs;
  venv

(** Translation of programs *)

let base_venv () =
  let prims = [
    "print",      [TIGstring],                  TIGvoid,    Pprint;
    "printi",     [TIGint],                     TIGvoid,    Pprinti;
    "flush",      [],                           TIGvoid,    Pflush;
    "getchar",    [],                           TIGstring,  Pgetchar;
    "ord",        [TIGstring],                  TIGint,     Pord;
    "chr",        [TIGint],                     TIGstring,  Pchr;
    "size",       [TIGstring],                  TIGint,     Psize;
    "substring",  [TIGstring; TIGint; TIGint],  TIGstring,  Psubstring;
    "concat",     [TIGstring; TIGstring],       TIGstring,  Pconcat;
    "not",        [TIGint],                     TIGint,     Pnot;
    "exit",       [TIGint],                     TIGvoid,    Pexit ] in
  let venv = Env.create () in
  List.iter (fun (name, ts, t, p) ->
    Env.add_primitive venv name (ts, t) p) prims;
  venv

let error_message = function
  | Unbound_variable x -> Printf.sprintf "undefined variable '%s'" x
  | Unbound_function x -> Printf.sprintf "undefined function '%s'" x
  | Unbound_type x -> Printf.sprintf "undefined type '%s'" x
  | Expected_array_type -> "expected array type"
  | Expected_record_type -> "expected record type"
  | Unknown_field x -> Printf.sprintf "unknown field '%s'" x
  | Expected_array -> "expected array"
  | Expected_record -> "expected record"
  | Type_mismatch(t, t0) ->
      Printf.sprintf "expected type '%s', found '%s'" (describe_type t) (describe_type t0)
  | Bad_arity(n, n0) ->
      Printf.sprintf "incorrect number of arguments, expected %d, found %d" n n0
  | Illegal_nil ->
      "'nil' should only be used in a context where its type can \
      be determined"
  | Illegal_break ->
      "'break' should only be used a 'while' or a 'for' loop"
  | Expected_record_array_elements ->
      "expected array to have elements of record type"
  | Expected_record_field ->
      "expected field to be of record type"
  | Unexpected_field x ->
      Printf.sprintf "unexpected field '%s'" x
  | Not_enough_fields -> "not enough fields"
  | Too_many_fields -> "too many fields"
  | Bad_cyclic_types ->
      "cyclic type declaration does not pass through a record type"

let transl_program e =
  try
    let venv = base_venv () in
    let e, _ = type_exp base_tenv venv false e in
    let max_static_depth = Env.max_static_depth venv in
    let frame_size = Env.frame_size venv in
    Printcode.print_code Format.std_formatter e;
    max_static_depth, frame_size, e
  with
    Error(p, reason) ->
      raise (Error.Error(p, error_message reason))
