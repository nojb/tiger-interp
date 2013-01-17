open Tiger_types
open Tiger_code
open Tiger_ast

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

exception Error of Tiger_loc.t * error

module M = Map.Make (String)

let find_variable (x, loc) venv =
  try
    let vd = Tiger_env.find_variable venv x in
    vd.Tiger_env.var_type, vd.Tiger_env.var_depth, vd.Tiger_env.var_frame_index
  with
    Not_found -> raise (Error (loc, Unbound_variable x))

let find_function (x, loc) venv =
  try
    Tiger_env.find_function venv x
  with
    Not_found -> raise (Error (loc, Unbound_function x))
  
let find_type (x, loc) tenv =
  try
    M.find x tenv
  with
    Not_found -> raise (Error (loc, Unbound_type x))

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

let find_array_type (x, loc) tenv =
  match find_type (x, loc) tenv with
  | TIGarray (t', _) as t -> t, t'
  | _ -> raise (Error (loc, Expected_array_type))

let find_record_type (x, loc) tenv =
  match find_type (x, loc) tenv with
  | TIGrecord (ts, _) as t -> t, ts
  | _ -> raise (Error (loc, Expected_record_type))

let find_record_field (x, loc) ts =
  let rec loop i =
    if i >= Array.length ts then
      raise (Error (loc, Unknown_field x))
    else if fst ts.(i) = x then i, snd ts.(i)
    else loop (i+1)
  in loop 0

(** Translation of variables *)

let rec array_var tenv venv inloop (v, loc) =
  let v, t = type_exp tenv venv inloop (v, loc) in
  match t with
  | TIGarray (t', _) -> v, t'
  | _ -> raise (Error (loc, Expected_array))

and record_var tenv venv inloop (v, loc) x =
  let v, t = type_exp tenv venv inloop (v, loc) in
  match t with
  | TIGrecord (ts, _) ->
      let rec loop i =
        let (x', t) = ts.(i) in
        if x' = x then v, i, t
        else loop (i+1)
      in loop 0
  | _ -> raise (Error (loc, Expected_record))

(** Translation of expressions *)

and exp_with_type tenv venv inloop (e, loc) t' =
  let e, t = type_exp tenv venv inloop (e, loc) in
  if type_equal t t' then e
  else raise (Error (loc, Type_mismatch))

and int_exp tenv venv inloop (e, loc) =
  let e, t = type_exp tenv venv inloop (e, loc) in
  match t with
  | TIGint -> e
  | _ -> raise (Error (loc, Expected_int))

and void_exp tenv venv inloop (e, loc) =
  let e, t = type_exp tenv venv inloop (e, loc) in
  match t with
  | TIGvoid -> e
  | _ -> raise (Error (loc, Expected_void))

and transl_call tenv venv inloop (x, loc) xs =
  let func = Tiger_env.find_function venv x in
  let (ts, t) = func.Tiger_env.fn_signature in

  if List.length xs <> List.length ts then
    raise (Error (loc, Bad_arity (List.length ts, List.length xs)));

  let rec loop args xs ts =
    match xs, ts with
    | [], [] ->
        let args = Array.of_list (List.rev args) in
        begin match func.Tiger_env.fn_desc with
        | Tiger_env.User f -> Ccall (f, args)
        | Tiger_env.Builtin p -> Cprim (p, args) end
    | x :: xs, t :: ts ->
        let x = exp_with_type tenv venv inloop x t in
        loop (x :: args) xs ts
    | _ -> assert false in
  (loop [] xs ts), t

and type_exp tenv venv inloop (e, loc) =
  match e with
  | Eint n ->
      Cquote (Vint n), TIGint
  | Estring s ->
      Cquote (Vstring s), TIGstring
  | Enil ->
      raise (Error (loc, Illegal_nil))
  | Esimple x ->
      let t, d, i = find_variable x venv in
      Cget (d, i), t
  | Eload (v, x) ->
      let v, t = array_var tenv venv inloop v in
      let x = int_exp tenv venv inloop x in
      Cload (v, x), t
  | Eget (v, (x, _)) ->
      let v, i, t = record_var tenv venv inloop v x in
      Cgetf (v, i), t
  | Ebinop (x, Op_add, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = int_exp tenv venv inloop y in
      Cadd (e1, e2), TIGint
  | Ebinop (x, Op_sub, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = int_exp tenv venv inloop y in
      Csub (e1, e2), TIGint
  | Ebinop (x, Op_mul, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = int_exp tenv venv inloop y in
      Cmul (e1, e2), TIGint
  | Ebinop (x, Op_eq, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = int_exp tenv venv inloop y in
      Cicmp (e1, Ceq, e2), TIGint
  | Ebinop (x, Op_leq, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = int_exp tenv venv inloop y in
      Cicmp (e1, Cle, e2), TIGint
  | Ebinop (x, Op_and, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = int_exp tenv venv inloop y in
      Candalso (e1, e2), TIGint
  | Ebinop (x, Op_or, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = int_exp tenv venv inloop y in
      Corelse (e1, e2), TIGint
  | Ebinop _ ->
      failwith "binop not implemented"
  (* | P.Eassign (v, (P.Enil, _)) -> FIXME NIL FIXME
      let v,  _ = record_var var v in
      Eassign (v, Eint 0), Tvoid *)
  | Eassign (x, e) ->
      let t, d, i = find_variable x venv in
      let e = exp_with_type tenv venv inloop e t in
      Cset (d, i, e), TIGvoid
  | Estore (v, x, e) ->
      let v, t = array_var tenv venv inloop v in
      let x = int_exp tenv venv inloop x in
      let e = exp_with_type tenv venv inloop e t in
      Cstore (v, x, e), TIGvoid
  | Eput (v, (x, _), e) ->
      let v, i, t = record_var tenv venv inloop v x in
      let e = exp_with_type tenv venv inloop e t in
      Csetf (v, i, e), TIGvoid
  | Ecall (x, xs) ->
      (* FIXME loc *)
      transl_call tenv venv inloop x xs
  | Eseq (xs) ->
      let rec loop es = function
        | [] -> Cseq [| |], TIGvoid
        | [x] ->
            let e, t = type_exp tenv venv inloop x in
            Cseq (Array.of_list (List.rev (e :: es))), t
        | x :: x' ->
            let e, _ = type_exp tenv venv inloop x in
            loop (e :: es) x' in
      loop [] xs
  | Earray (x, y, z) -> (* FIXME typecheck *)
      let y = int_exp tenv venv inloop y in
      let z, t = type_exp tenv venv inloop z in
      Cmakearray (y, z), find_type x tenv
  | Erecord (t, xs) -> (* FIXME typecheck *)
      Cmakerecord (Array.of_list
        (List.map (fun (_, x) ->
          let e, _ = type_exp tenv venv inloop x in e) xs)), find_type t tenv
  | Eif (x, y, None) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = void_exp tenv venv inloop y in
      Cif (e1, e2, Cquote Vunit), TIGvoid
  | Eif (x, y, Some z) ->
      let e1 = int_exp tenv venv inloop x in
      let e2, t2 = type_exp tenv venv inloop y in
      let e3, t3 = type_exp tenv venv inloop z in
      (* FIXME check types *)
      Cif (e1, e2, e3), t2
  | Ewhile (x, y) ->
      let e1 = int_exp tenv venv inloop x in
      let e2 = void_exp tenv venv true y in
      Cwhile (e1, e2), TIGvoid
  | Efor (i, x, y, z) ->
      let x = int_exp tenv venv inloop x in
      let y = int_exp tenv venv inloop y in
      let venv = Tiger_env.new_scope venv in
      let d, i = Tiger_env.add_variable venv i TIGint in
      let z = void_exp tenv venv true z in
      Cfor (d, i, x, y, z), TIGvoid
  | Ebreak ->
      if inloop then Cbreak, TIGvoid
      else raise (Error (loc, Illegal_break))
  | Eletvar (x, None, y, z) ->
      let y, t = type_exp tenv venv inloop y in
      let venv = Tiger_env.new_scope venv in
      let d, i = Tiger_env.add_variable venv x t in
      let z, t = type_exp tenv venv inloop z in
      Cseq [| Cset (d, i, y); z |], t
  | Eletvar (x, Some t, (Enil, _), z) ->
      let t, _ = find_record_type t tenv in
      let venv = Tiger_env.new_scope venv in
      let d, i = Tiger_env.add_variable venv x t in
      let z, t = type_exp tenv venv inloop z in
      Cseq [| Cset (d, i, Cquote (Vrecord None)); z |], t
  | Eletvar (x, Some t, y, z) ->
      let t = find_type t tenv in
      let y = exp_with_type tenv venv inloop y t in
      let venv = Tiger_env.new_scope venv in
      let d, i = Tiger_env.add_variable venv x t in
      let z, t = type_exp tenv venv inloop z in
      Cseq [| Cset (d, i, y); z |], t
  | Elettype (tys, e) ->
      type_exp (lettype tenv tys) venv inloop e
  | Eletfuns (funs, e) ->
      let venv = letfuns tenv venv funs in
      type_exp tenv venv inloop e

(** Translation of types *)

and lettype tenv tys =
  let deftype (x, t) =
    match t with
    | Tname (y, _) -> begin
        try (x, M.find y tenv)
        with Not_found -> (x, TIGforward y)
      end
    | Tarray y ->
        (x, new_array_type (find_type y tenv))
    | Trecord xs ->
        let field ((x, _), (t, _)) =
          try (x, M.find t tenv)
          with Not_found -> (x, TIGforward t)
        in
        (x, new_record_type (Array.of_list (List.map field xs)))
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
  let (name, _) = fn.fun_name in
  name

and letfuns tenv venv funs =

  let declare_function venv func =
    Tiger_env.add_function venv
      (function_name func)
      (function_signature tenv func) in

  let update_function fn venv body =
    match fn.Tiger_env.fn_desc with
    | Tiger_env.User p ->
        p.proc_code <- body;
        p.proc_frame_size <- Tiger_env.frame_size venv
    | _ -> assert false in

  let define_function venv func =
    let fn = Tiger_env.find_function venv (fst func.fun_name) in
    let ts, t = fn.Tiger_env.fn_signature in
    let venv = Tiger_env.new_frame venv in
    List.iter2 (fun ((x, _), _) t ->
      ignore (Tiger_env.add_variable venv x t)) func.fun_args ts;
    let body = exp_with_type tenv venv false func.fun_body t in
    update_function fn venv body in

  let venv = Tiger_env.new_scope venv in
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
  let venv = Tiger_env.create () in
  List.iter (fun (name, ts, t, p) ->
    Tiger_env.add_primitive venv name (ts, t) p) prims;
  venv

let transl_program e =
  type_count := 0;
  let venv = base_venv () in
  let e, _ = type_exp base_tenv venv false e in
  let max_static_depth = Tiger_env.max_static_depth venv in
  let frame_size = Tiger_env.frame_size venv in
  Printcode.print_code Format.std_formatter e;
  max_static_depth, frame_size, e
