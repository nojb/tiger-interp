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

open Code

type runtime_error =
  | Out_of_bounds
  | Nil_record
  | Div_by_zero

exception Runtime_error of int * runtime_error
exception Break
exception Exit of int

module M = Map.Make(String)

let eval_primitive p va =
  match p, va with
  | Pprint, [Vstring s] ->
      print_string s;
      Vunit
  | Pprinti, [Vint i] ->
      print_int i;
      Vunit
  | Pflush, [] ->
      flush stdout;
      Vunit
  | Pgetchar, [] ->
      Vstring (try String.make 1 (input_char stdin) with End_of_file -> "")
  | Pord, [Vstring ""] ->
      Vint (-1)
  | Pord, [Vstring s] ->
      Vint (int_of_char s.[0])
  | Pchr, [Vint i] ->
      Vstring (String.make 1 (char_of_int i))
  | Psize, [Vstring s] ->
      Vint (String.length s)
  | Psubstring, [Vstring s; Vint f; Vint n] ->
      Vstring (String.sub s f n)
  | Pconcat, [Vstring s1; Vstring s2] ->
      Vstring (s1 ^ s2)
  | Pnot, [Vint 0] ->
      Vint 1
  | Pnot, [Vint _] ->
      Vint 0
  | Pexit, [Vint i] ->
      raise (Exit i)
  | _ ->
      assert false

let load lnum v1 v2 =
  match v1, v2 with
  | Varray a, Vint n ->
      if n >= Array.length a || n < 0 then
        raise (Runtime_error(lnum, Out_of_bounds))
      else
        a.(n)
  | _ -> assert false

let store lnum v1 v2 v3 =
  match v1, v2 with
  | Varray a, Vint n ->
      if n >= Array.length a || n < 0 then
        raise (Runtime_error(lnum, Out_of_bounds))
      else
        a.(n) <- v3
  | _ -> assert false

let getf lnum v1 i =
  match v1 with
  | Vrecord None -> raise (Runtime_error(lnum, Nil_record))
  | Vrecord (Some a) -> a.(i)
  | _ -> assert false

let setf lnum v1 i v2 =
  match v1 with
  | Vrecord None -> raise (Runtime_error(lnum, Nil_record))
  | Vrecord (Some a) -> a.(i) <- v2
  | _ -> assert false

let add e1 e2 =
  match e1, e2 with
  | Vint n, Vint m -> Vint (n + m)
  | _ -> assert false

let mul e1 e2 =
  match e1, e2 with
  | Vint n, Vint m -> Vint (n * m)
  | _ -> assert false

let div lnum e1 e2 =
  match e1, e2 with
  | Vint _, Vint 0 -> raise (Runtime_error(lnum, Div_by_zero))
  | Vint n, Vint m -> Vint (n / m)
  | _ -> assert false

let sub e1 e2 =
  match e1, e2 with
  | Vint n, Vint m -> Vint (n - m)
  | _ -> assert false

let icmp v1 cmp v2 =
  match v1, cmp, v2 with
  | Vint n, Ceq, Vint m -> Vint (if n  = m then 1 else 0)
  | Vint n, Cne, Vint m -> Vint (if n <> m then 1 else 0)
  | Vint n, Clt, Vint m -> Vint (if n  < m then 1 else 0)
  | Vint n, Cle, Vint m -> Vint (if n <= m then 1 else 0)
  | Vint n, Cge, Vint m -> Vint (if n >= m then 1 else 0)
  | Vint n, Cgt, Vint m -> Vint (if n  > m then 1 else 0)
  | _ -> assert false

let scmp v1 cmp v2 =
  match v1, cmp, v2 with
  | Vstring s1, Ceq, Vstring s2 -> Vint (if s1  = s2 then 1 else 0)
  | Vstring s1, Cne, Vstring s2 -> Vint (if s1 <> s2 then 1 else 0)
  | Vstring s1, Clt, Vstring s2 -> Vint (if s1  < s2 then 1 else 0)
  | Vstring s1, Cle, Vstring s2 -> Vint (if s1 <= s2 then 1 else 0)
  | Vstring s1, Cge, Vstring s2 -> Vint (if s1 >= s2 then 1 else 0)
  | Vstring s1, Cgt, Vstring s2 -> Vint (if s1  > s2 then 1 else 0)
  | _ -> assert false

let pcmp v1 cmp v2 =
  match v1, cmp, v2 with
  | Varray a1, Ceq, Varray a2 -> Vint (if a1 == a2 then 1 else 0)
  | Varray a1, Cne, Varray a2 -> Vint (if a1 != a2 then 1 else 0)
  | Vrecord r1, Ceq, Vrecord r2 -> Vint (if r1 == r2 then 1 else 0)
  | Vrecord r1, Cne, Vrecord r2 -> Vint (if r1 != r2 then 1 else 0)
  | _ -> assert false

let rec eval_int env funs e =
  match eval env funs e with
  | Vint n -> n
  | _ -> assert false

and eval env funs = function
  | Cquote v ->
      v
  | Cvar id ->
      !(M.find id env)
  | Cassign (id, e) ->
      M.find id env := eval env funs e;
      Vunit
  | Cload (lnum, e1, e2) ->
      load lnum (eval env funs e1) (eval env funs e2)
  | Cstore (lnum, e1, e2, e3) ->
      store lnum (eval env funs e1) (eval env funs e2) (eval env funs e3);
      Vunit
  | Cgetf (lnum, e1, i) ->
      getf lnum (eval env funs e1) i
  | Csetf (lnum, e1, i, e2) ->
      setf lnum (eval env funs e1) i (eval env funs e2);
      Vunit
  | Cadd (e1, e2) ->
      add (eval env funs e1) (eval env funs e2)
  | Cmul (e1, e2) ->
      mul (eval env funs e1) (eval env funs e2)
  | Cdiv (lnum, e1, e2) ->
      div lnum (eval env funs e1) (eval env funs e2)
  | Csub (e1, e2) ->
      sub (eval env funs e1) (eval env funs e2)
  | Cicmp (e1, cmp, e2) ->
      icmp (eval env funs e1) cmp (eval env funs e2)
  | Cscmp (e1, cmp, e2) ->
      scmp (eval env funs e1) cmp (eval env funs e2)
  | Cpcmp (e1, cmp, e2) ->
      pcmp (eval env funs e1) cmp (eval env funs e2)
  | Ccall (p, ea) ->
      let args, body = M.find p funs in
      let env' = env in
      let env =
        List.fold_left2 (fun env id e ->
            M.add id (ref (eval env' funs e)) env
          ) env' args ea
      in
      eval env funs body
  | Cseq (e1, e2) ->
      let (_ : value) = eval env funs e1 in
      eval env funs e2
  | Cmakearray (e1, e2) ->
      Varray (Array.make (eval_int env funs e1) (eval env funs e2))
  | Cmakerecord(ea) ->
      Vrecord (Some (Array.of_list (List.map (eval env funs) ea)))
  | Cif (e1, e2, e3) ->
      eval env funs (if eval_int env funs e1 <> 0 then e2 else e3)
  | Cwhile (e1, e2) ->
      let rec loop v1 =
        if v1 <> 0 then begin
            ignore (eval env funs e2);
            loop (eval_int env funs e1)
        end
      in begin try
        loop (eval_int env funs e1);
        Vunit
      with
        Break -> Vunit
      end
  | Cfor (id, e1, e2, e3) ->
      let v1 = eval_int env funs e1 in
      let v2 = eval_int env funs e2 in
      let r = M.find id env in
      let rec loop u =
        if u <= v2 then begin
          r := Vint u;
          ignore (eval env funs e3);
          loop (u + 1)
        end
      in
      begin try loop v1; Vunit with Break -> Vunit end
  | Cbreak ->
      raise Break
  | Cprim (p, ea) ->
      eval_primitive p (List.map (eval env funs) ea)
  | Cletrec (fs, e) ->
      let funs =
        List.fold_left (fun funs (name, args, body) -> M.add name (args, body) funs) funs fs
      in
      eval env funs e
  | Clet (id, e1, e2) ->
      eval (M.add id (ref (eval env funs e1)) env) funs e2

let run e =
  eval M.empty M.empty e
