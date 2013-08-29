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
  | Type_mismatch
  | Expected_int
  | Expected_void
  | Bad_arity of int * int
  | Illegal_nil
  | Illegal_break

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

let rec resolve_ty tenv t =
  let cache = ref [] in
  let rec loop t =
    if List.exists (fun t' -> t' == t) !cache then t
    else begin
      cache := t :: !cache;
      match t with
      | TIGarray (t, id) ->
          TIGarray (loop t, id)
      | TIGrecord (ts, id) ->
          TIGrecord (Array.map (fun (x, t) -> x, loop t) ts, id)
      | TIGforward name -> begin
          try M.find name tenv
          with Not_found -> failwith "undefined forward ref"
        end
      | _ as t ->
          t
    end
  in loop t

let find_array_type x tenv =
  match find_type x tenv with
  | TIGarray (t', _) as t -> t, t'
  | _ -> raise (Error (x.pos, Expected_array_type))

let find_record_type x tenv =
  match find_type x tenv with
  | TIGrecord (ts, _) as t -> t, ts
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
  match t with
  | TIGarray(t', _) -> v', t'
  | _ -> raise (Error(var_pos v, Expected_array))

and record_var tenv venv inloop v x =
  let v', t = type_var tenv venv inloop v in
  match t with
  | TIGrecord(ts, _) ->
      let rec loop i =
        let (x', t) = ts.(i) in
        if x' = x.id then v', i, t
        else loop (i+1)
      in loop 0
  | _ -> raise (Error(var_pos v, Expected_record))

(** Translation of expressions *)

and exp_with_type tenv venv inloop (e : exp) t' : Code.code =
  let e', t = type_exp tenv venv inloop e in
  if type_equal t t' then e'
  else raise (Error(exp_pos e, Type_mismatch))

and int_exp tenv venv inloop e =
  let e', t = type_exp tenv venv inloop e in
  match t with
  | TIGint -> e'
  | _ -> raise (Error(exp_pos e, Expected_int))

and void_exp tenv venv inloop e =
  let e', t = type_exp tenv venv inloop e in
  match t with
  | TIGvoid -> e'
  | _ -> raise (Error(exp_pos e, Expected_void))

and transl_call tenv venv inloop x xs =
  let func = Env.find_function venv x.id in
  let (ts, t) = func.Env.fn_signature in

  if List.length xs <> List.length ts then
    raise (Error(x.pos, Bad_arity (List.length ts, List.length xs)));

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
  | Vsubscript(_, v, e) ->
      let v, t = array_var tenv venv inloop v in
      let x = int_exp tenv venv inloop e in
      Cload(v, x), t
  | Vfield(_, v, id) ->
      let v, i, t = record_var tenv venv inloop v id in
      Cgetf(v, i), t

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
  | Ebinop(_, x, Op_div, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = int_exp tenv venv inloop y in
      Cdiv(e1, e2), TIGint
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
  | Ebinop(_, x, Op_and, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = int_exp tenv venv inloop y in
      Candalso(e1, e2), TIGint
  | Ebinop(_, x, Op_or, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = int_exp tenv venv inloop y in
      Corelse(e1, e2), TIGint
  (* | P.Eassign (v, (P.Enil, _)) -> FIXME NIL FIXME
      let v,  _ = record_var var v in
      Eassign (v, Eint 0), Tvoid *)
  | Eassign(_, Vsimple id, e) ->
      let t, d, i = find_variable id venv in
      let e = exp_with_type tenv venv inloop e t in
      Cset(d, i, e), TIGvoid
  | Eassign(_, Vsubscript(_, v, x), e) ->
      let v, t = array_var tenv venv inloop v in
      let x = int_exp tenv venv inloop x in
      let e = exp_with_type tenv venv inloop e t in
      Cstore (v, x, e), TIGvoid
  | Eassign(_, Vfield(_, v, x), e) ->
      let v, i, t = record_var tenv venv inloop v x in
      let e = exp_with_type tenv venv inloop e t in
      Csetf (v, i, e), TIGvoid
  | Ecall(_, x, xs) ->
      (* FIXME loc *)
      transl_call tenv venv inloop x xs
  | Eseq(x, y) ->
      let x, _ = type_exp tenv venv inloop x in
      let y, t = type_exp tenv venv inloop y in
      Cseq(x, y), t
  | Earray(_, x, y, z) -> (* FIXME typecheck *)
      let y = int_exp tenv venv inloop y in
      let z, t = type_exp tenv venv inloop z in
      Cmakearray(y, z), find_type x tenv
  | Erecord(_, t, xs) -> (* FIXME typecheck *)
      Cmakerecord(Array.of_list
        (List.map (fun (_, x) ->
          let e, _ = type_exp tenv venv inloop x in e) xs)), find_type t tenv
  | Eif(_, x, y, z) ->
      let e1 = int_exp tenv venv inloop x in
      let e2, t2 = type_exp tenv venv inloop y in
      let e3, t3 = type_exp tenv venv inloop z in
      (* FIXME check types *)
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
  let deftype (x, t) =
    match t with
    | Tname (y) -> begin
        try (x.id, M.find y.id tenv)
        with Not_found -> (x.id, TIGforward y.id)
      end
    | Tarray y ->
        (x.id, new_array_type (find_type y tenv))
    | Trecord xs ->
        let field (x, t) =
          try (x.id, M.find t.id tenv)
          with Not_found -> (x.id, TIGforward t.id)
        in
        (x.id, new_record_type (Array.of_list (List.map field xs)))
  in
  let tys = List.map deftype tys in
  let tenv = List.fold_left (fun tenv (x, t) ->
    M.add x t tenv) tenv tys in
  let tenv = List.fold_left (fun tenv (x, t) ->
    M.add x (resolve_ty tenv t) tenv) tenv tys in
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
    "exit",       [TIGint],                     TIGvoid,    Pexit;
    "sizea",      [TIGanyarray],                TIGint,     Psizea ] in
  let venv = Env.create () in
  List.iter (fun (name, ts, t, p) ->
    Env.add_primitive venv name (ts, t) p) prims;
  venv

let transl_program e =
  type_count := 0;
  let venv = base_venv () in
  let e, _ = type_exp base_tenv venv false e in
  let max_static_depth = Env.max_static_depth venv in
  let frame_size = Env.frame_size venv in
  Printcode.print_code Format.std_formatter e;
  max_static_depth, frame_size, e
