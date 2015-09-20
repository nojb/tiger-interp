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

open Format
open Code

let string_of_comparison = function
  | Ceq -> "="
  | Cne -> "!="
  | Cle -> "<="
  | Clt -> "<"
  | Cgt -> ">"
  | Cge -> ">="

let string_of_primitive = function
  | Pprint      -> "print"
  | Pprinti     -> "printi"
  | Pflush      -> "flush"
  | Pgetchar    -> "getchar"
  | Pord        -> "ord"
  | Pchr        -> "chr"
  | Psize       -> "size"
  | Psubstring  -> "substring"
  | Pconcat     -> "concat"
  | Pnot        -> "not"
  | Pexit       -> "exit"

let rec value ppf = function
  | Vunit -> fprintf ppf "()"
  | Vint n -> fprintf ppf "%d" n
  | Vstring s -> fprintf ppf "%S" s
  | Varray va ->
      let values ppf va =
        Array.iter (fun v -> fprintf ppf "@ %a" value v) va in
      fprintf ppf "@[<2>#(%a)@]" values va
  | Vrecord None ->
      fprintf ppf "#()" (* or "()" ? *)
  | Vrecord (Some va) ->
      let values ppf va =
        Array.iter (fun v -> fprintf ppf "@ %a" value v) va in
      fprintf ppf "@[<2>#(%a)@]" values va

let pp_list pp ppf = function
  | [] -> ()
  | [x] -> pp ppf x
  | x :: xs ->
      fprintf ppf "%a" pp x;
      List.iter (fprintf ppf "@ %a" pp) xs

let rec code ppf = function
  | Cquote v ->
      value ppf v
  | Cvar id ->
      fprintf ppf "%s" id
  | Cassign (id, c) ->
      fprintf ppf "@[<2>(assign %s@ %a)@]" id code c
  | Cload (lnum, c1, c2) ->
      fprintf ppf "@[<2>(load@ [%d]@ %a@ %a)@]" lnum code c1 code c2
  | Cstore (lnum, c1, c2, c3) ->
      fprintf ppf "@[<2>(store@ [%d]@ %a@ %a@ %a)@]" lnum code c1 code c2 code c3
  | Cgetf (lnum, c1, i) ->
      fprintf ppf "@[<2>(getf@ [%d]@ %a@ %d)@]" lnum code c1 i
  | Csetf (lnum, c1, i, c2) ->
      fprintf ppf "@[<2>(setf@ [%d]@ %a@ %d@ %a)@]" lnum code c1 i code c2
  | Cadd (c1, c2) ->
      fprintf ppf "@[<2>(+@ %a@ %a)@]" code c1 code c2
  | Cmul (c1, c2) ->
      fprintf ppf "@[<2>(*@ %a@ %a)@]" code c1 code c2
  | Cdiv (lnum, c1, c2) ->
      fprintf ppf "@[<2>(/@ [%d]@ %a@ %a)@]" lnum code c1 code c2
  | Csub (c1, c2) ->
      fprintf ppf "@[<2>(-@ %a@ %a)@]" code c1 code c2
  | Cicmp (c1, cmp, c2)
  | Cscmp (c1, cmp, c2)
  | Cpcmp (c1, cmp, c2) ->
      fprintf ppf "@[<2>(%s@ %a@ %a)@]"
        (string_of_comparison cmp) code c1 code c2
  | Ccall (p, ca) ->
      let args ppf ca = List.iter (fun a -> fprintf ppf "@ %a" code a) ca in
      fprintf ppf "@[<2>(%s%a)@]" p args ca
  | Cseq _ as c ->
      let rec seq ppf = function
        | Cseq (c1, c2) -> fprintf ppf "%a@ %a" seq c1 seq c2
        | c -> code ppf c
      in
      fprintf ppf "@[<2>(seq@ %a)@]" seq c
  | Cmakearray (c1, c2) ->
      fprintf ppf "@[<2>(makearray@ %a@ %a)@]" code c1 code c2
  | Cmakerecord ca ->
      let fields ppf ca = List.iter (fun f -> fprintf ppf "@ %a" code f) ca in
      fprintf ppf "@[<2>(makerecord%a)@]" fields ca
  | Cif (c1, c2, c3) ->
      fprintf ppf "@[<2>(if@ %a@ %a@ %a)@]" code c1 code c2 code c3
  | Cwhile (c1, c2) ->
      fprintf ppf "@[<2>(while@ %a@ %a)@]" code c1 code c2
  | Cfor (id, c1, c2, c3) ->
      fprintf ppf "@[<2>(for %s@ %a@ %a@ %a)@]" id code c1 code c2 code c3
  | Cbreak ->
      fprintf ppf "(break)"
  | Cprim (p, ca) ->
      let codes ppf args = List.iter (fun a -> fprintf ppf "@ %a" code a) args in
      fprintf ppf "@[<2>(%s%a)@]" (string_of_primitive p) codes ca
  | Cletrec (funs, e) ->
      let pp_fun ppf (name, args, body) =
        fprintf ppf "@[<2>%s@ (%a)@ %a@]" name (pp_list Format.pp_print_string) args code body
      in
      fprintf ppf "@[<2>(letrec (%a)@ %a)@]" (pp_list pp_fun) funs code e
  | Clet (id, e1, e2) ->
      let rec aux ppf = function
        | Clet (id, e1, e2) ->
            fprintf ppf "@ %s@ %a%a" id code e1 aux e2
        | c ->
            fprintf ppf ")@ %a" code c
      in
      fprintf ppf "@[<2>(let@ (%s@ %a%a@]" id code e1 aux e2

let print_code ppf c =
  fprintf ppf "@[<2>%a@]@." code c;
