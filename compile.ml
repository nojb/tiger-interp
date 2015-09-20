(* The MIT License (MIT)

   Copyright (c) 2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Types
open Code
open Ast

type error =
  | Unbound_variable of string
  | Unbound_function of string
  | Unbound_type of string
  | Expected_array_type of tiger_type
  | Expected_record_type of tiger_type
  | Unknown_field of string
  | Expected_array of tiger_type
  | Expected_record of tiger_type
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
  | Illegal_comparison of cmp

exception Error of Lexing.position * error

let cmp_op = function
  | Eq -> Ceq
  | Ne -> Cne
  | Le -> Cle
  | Lt -> Clt
  | Ge -> Cge
  | Gt -> Cgt

module M = Map.Make (String)

let find_variable x venv =
  try
    Env.find_variable venv x.id
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

let check_type tenv (x, _) =
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
  | _ as t -> raise (Error (x.pos, Expected_array_type t))

let find_record_type x tenv =
  match unroll tenv (find_type x tenv) with
  | TIGrecord (_, ts) as t -> t, ts
  | _ as t -> raise (Error (x.pos, Expected_record_type t))

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
  | _ as t -> raise (Error(var_pos v, Expected_array t))

and record_var tenv venv inloop v x =
  let v', t = type_var tenv venv inloop v in
  match unroll tenv t with
  | TIGrecord(_, ts) ->
      let rec loop i = function
        | [] -> raise (Error(x.pos, Unexpected_field x.id))
        | (x', t) :: _ when x.id = x' -> v', i, t
        | _ :: xts -> loop (i+1) xts
      in loop 0 ts
  | _ as t -> raise (Error(var_pos v, Expected_record t))

(** Translation of expressions *)

and exp_with_type tenv (venv : Env.env) inloop (e : exp) t' : Code.code =
  let e', t = type_exp tenv venv inloop e in
  if type_equal tenv t t' then e'
  else raise (Error(exp_pos e, Type_mismatch(unroll tenv t, unroll tenv t')))

and int_exp tenv venv inloop e =
  exp_with_type tenv venv inloop e TIGint

and void_exp tenv venv inloop e =
  exp_with_type tenv venv inloop e TIGvoid

and transl_call tenv venv inloop x xs =
  let func = find_function x venv in
  let (ts, t) = func.Env.fn_signature in

  if List.length xs <> List.length ts then
    raise (Error(x.pos, Bad_arity(List.length ts, List.length xs)));

  let rec loop args xs ts =
    match xs, ts with
    | [], [] ->
        begin match func.Env.fn_desc with
          | Env.User -> Ccall (x.id, List.rev args)
          | Env.Builtin p -> Cprim (p, args)
        end
    | Enil p :: xs, t :: ts ->
        begin match unroll tenv t with
        | TIGrecord _ ->
            loop (Cquote(Vrecord None) :: args) xs ts
        | _ as t ->
            raise (Error(p, Expected_record_type t))
        end
    | x :: xs, t :: ts ->
        let x = exp_with_type tenv venv inloop x t in
        loop (x :: args) xs ts
    | _ ->
        assert false in
  (loop [] xs ts), t

and type_var tenv venv inloop v =
  match v with
  | Vsimple id ->
      let t = find_variable id venv in
      Cvar id.id, t
  | Vsubscript(p, v, e) ->
      let v, t = array_var tenv venv inloop v in
      let x = int_exp tenv venv inloop e in
      Cload(p.Lexing.pos_lnum, v, x), t
  | Vfield(p, v, id) ->
      let v, i, t = record_var tenv venv inloop v id in
      Cgetf(p.Lexing.pos_lnum, v, i), t

and type_exp tenv (venv : Env.env) inloop (e : exp) : Code.code * Types.tiger_type =
  match e with
  | Eunit _ ->
      Cquote Vunit, TIGvoid
  | Eint (_, n) ->
      Cquote(Vint n), TIGint
  | Estring (_, s) ->
      Cquote(Vstring s), TIGstring
  | Enil p ->
      raise (Error(p, Illegal_nil))
  | Evar v ->
      type_var tenv venv inloop v
  | Ebinop (_, x, Op_add, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = int_exp tenv venv inloop y in
      Cadd(e1, e2), TIGint
  | Ebinop (_, x, Op_sub, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = int_exp tenv venv inloop y in
      Csub(e1, e2), TIGint
  | Ebinop (_, x, Op_mul, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = int_exp tenv venv inloop y in
      Cmul(e1, e2), TIGint
  | Ebinop (p, x, Op_div, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = int_exp tenv venv inloop y in
      Cdiv(p.Lexing.pos_lnum, e1, e2), TIGint
  | Ebinop (p, x, Ocmp cmp, Enil _)
  | Ebinop (p, Enil _, Ocmp cmp, x) ->
      let x, t = type_exp tenv venv inloop x in
      begin match unroll tenv t, cmp with
      | TIGrecord _, Eq
      | TIGrecord _, Ne -> Cpcmp(x, cmp_op cmp, Cquote(Vrecord None)), TIGint
      | TIGrecord _, _ -> raise (Error(p, Illegal_comparison cmp))
      | _ as t, _ -> raise (Error(p, Expected_record t))
      end
  | Ebinop (_, x, Ocmp cmp, y) ->
      let e1, t1 = type_exp tenv venv inloop x in
      let e2 = exp_with_type tenv venv inloop y t1 in
      begin match unroll tenv t1, cmp with
      | TIGint, _ -> Cicmp(e1, cmp_op cmp, e2), TIGint
      | TIGstring, _ -> Cscmp(e1, cmp_op cmp, e2), TIGint
      | TIGrecord _, Eq | TIGrecord _, Ne
      | TIGarray _, Eq | TIGarray _, Ne -> Cpcmp(e1, cmp_op cmp, e2), TIGint
      | _ -> assert false (* FIXME *)
      end
  | Eassign (_, Vsimple id, Enil _) ->
      let t = find_variable id venv in
      begin match unroll tenv t with
      | TIGrecord _ ->
          Cassign (id.id, Cquote(Vrecord None)), TIGvoid
      | _ as t ->
          raise (Error(id.pos, Expected_record t))
      end
  | Eassign (_, Vsimple id, e) ->
      let t = find_variable id venv in
      let e = exp_with_type tenv venv inloop e t in
      Cassign (id.id, e), TIGvoid
  | Eassign (p, Vsubscript(_, v, x), Enil _) ->
      let v', t = array_var tenv venv inloop v in
      begin match unroll tenv t with
      | TIGrecord _ ->
          let x = int_exp tenv venv inloop x in
          Cstore(p.Lexing.pos_lnum, v', x, Cquote(Vrecord None)), TIGvoid
      | _ ->
          raise (Error(var_pos v, Expected_record_array_elements))
      end
  | Eassign (p, Vsubscript(_, v, x), e) ->
      let v, t = array_var tenv venv inloop v in
      let x = int_exp tenv venv inloop x in
      let e = exp_with_type tenv venv inloop e t in
      Cstore (p.Lexing.pos_lnum, v, x, e), TIGvoid
  | Eassign (p, Vfield(_, v, x), Enil _) ->
      let v', i, t = record_var tenv venv inloop v x in
      begin match unroll tenv t with
      | TIGrecord _ ->
          Csetf(p.Lexing.pos_lnum, v', i, Cquote(Vrecord None)), TIGvoid
      | _ ->
          raise (Error(var_pos v, Expected_record_field))
      end
  | Eassign (p, Vfield(_, v, x), e) ->
      let v, i, t = record_var tenv venv inloop v x in
      let e = exp_with_type tenv venv inloop e t in
      Csetf(p.Lexing.pos_lnum, v, i, e), TIGvoid
  | Ecall (_, x, xs) ->
      transl_call tenv venv inloop x xs
  | Eseq (x, y) ->
      let x, _ = type_exp tenv venv inloop x in
      let y, t = type_exp tenv venv inloop y in
      Cseq(x, y), t
  | Earray (_, x, y, Enil _) ->
      let t, t' = find_array_type x tenv in
      begin match unroll tenv t' with
      | TIGrecord _ ->
          let y = int_exp tenv venv inloop y in
          Cmakearray(y, Cquote(Vrecord(None))), t
      | _ ->
          raise (Error(x.pos, Expected_record_array_elements))
      end
  | Earray (_, x, y, z) ->
      let t, t' = find_array_type x tenv in
      let y = int_exp tenv venv inloop y in
      let z = exp_with_type tenv venv inloop z t' in
      Cmakearray(y, z), t
  | Erecord (p, t, xs) ->
      let t, ts = find_record_type t tenv in
      let rec bind vs = function
        | [], [] ->
            Cmakerecord (List.rev vs), t
        | (x, Enil _) :: xts, (x', _) :: ts ->
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
  | Eif (_, x, y, z) ->
      let e1 = int_exp tenv venv inloop x in
      let e2, t2 = type_exp tenv venv inloop y in
      let e3 = exp_with_type tenv venv inloop z t2 in
      Cif(e1, e2, e3), t2
  | Ewhile (_, x, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = void_exp tenv venv true y in
      Cwhile(e1, e2), TIGvoid
  | Efor (_, i, x, y, z) ->
      let x = int_exp tenv venv inloop x in
      let y = int_exp tenv venv inloop y in
      let venv = Env.add_variable venv i.id TIGint in
      let z = void_exp tenv venv true z in
      Cfor (i.id, x, y, z), TIGvoid
  | Ebreak _ when inloop ->
      Cbreak, TIGvoid
  | Ebreak p ->
      raise (Error(p, Illegal_break))
  | Elet (_, Dvar(x, None, y), z) ->
      let y, t = type_exp tenv venv inloop y in
      let venv = Env.add_variable venv x.id t in
      let z, t = type_exp tenv venv inloop z in
      Clet (x.id, y, z), t
  | Elet (_, Dvar(x, Some t, Enil _), z) ->
      let t, _ = find_record_type t tenv in
      let venv = Env.add_variable venv x.id t in
      let z, t = type_exp tenv venv inloop z in
      Clet (x.id, Cquote(Vrecord None), z), t
  | Elet (_, Dvar(x, Some t, y), z) ->
      let t = find_type t tenv in
      let y = exp_with_type tenv venv inloop y t in
      let venv = Env.add_variable venv x.id t in
      let z, t = type_exp tenv venv inloop z in
      Clet (x.id, y, z), t
  | Elet (_, Dtypes tys, e) ->
      type_exp (lettype tenv tys) venv inloop e
  | Elet (_, Dfuns funs, e) ->
      type_letrec tenv venv inloop funs e

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

and type_letrec tenv venv inloop funs e =

  let signature_type fn =
    let ty = match fn.fun_rety with
      | None -> TIGvoid
      | Some t -> find_type t tenv in
    List.map (fun (_, t) -> find_type t tenv) fn.fun_args, ty
  in

  let function_name fn = fn.fun_name.id in

  let declare_function venv func =
    let tys, rty = signature_type func in
    Env.add_function venv (function_name func) tys rty
  in

  let define_function (venv : Env.env) func =
    let fn = Env.find_function venv func.fun_name.id in
    let ts, t = fn.Env.fn_signature in
    let venv =
      List.fold_left2 (fun venv (x, _) t -> Env.add_variable venv x.id t) venv func.fun_args ts
    in
    func.fun_name.id, List.map (fun (x, _) -> x.id) func.fun_args, exp_with_type tenv venv false func.fun_body t
  in

  let venv = List.fold_left declare_function venv funs in
  let body, t = type_exp tenv venv inloop e in
  Cletrec (List.map (define_function venv) funs, body), t

(** Translation of programs *)

let base_venv () =
  let prims =
    [
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
      "exit",       [TIGint],                     TIGvoid,    Pexit;
    ]
  in
  List.fold_left (fun venv (name, ts, t, p) ->
      Env.add_primitive venv name ts t p
    ) Env.empty prims

let error_message = function
  | Unbound_variable x -> Printf.sprintf "undefined variable '%s'" x
  | Unbound_function x -> Printf.sprintf "undefined function '%s'" x
  | Unbound_type x -> Printf.sprintf "undefined type '%s'" x
  | Expected_array_type t ->
      Printf.sprintf "expected array type name, instead found '%s'"
        (describe_type t)
  | Expected_record_type t ->
      Printf.sprintf "expected record type name, instead found '%s'"
        (describe_type t)
  | Unknown_field x -> Printf.sprintf "unknown field '%s'" x
  | Expected_array t ->
      Printf.sprintf "expected array variable, instead found type '%s'"
        (describe_type t)
  | Expected_record t ->
      Printf.sprintf "expected record variable, instead found type '%s'"
        (describe_type t)
  | Type_mismatch(t, t0) ->
      Printf.sprintf "expected type '%s', found '%s'" (describe_type t) (describe_type t0)
  | Bad_arity(n, n0) ->
      Printf.sprintf "incorrect number of arguments, expected %d, found %d" n n0
  | Illegal_nil ->
      "'nil' must be used in a context where its type can \
      be determined"
  | Illegal_break ->
      "'break' must be used a 'while' or a 'for' loop"
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
  | Illegal_comparison _ ->
      "illegal use of comparison operator"

let transl_program e =
  try
    let venv = base_venv () in
    let e, _ = type_exp base_tenv venv false e in
    Printcode.print_code Format.std_formatter e;
    e
  with
    Error(p, reason) ->
      raise (Error.Error(p, error_message reason))
