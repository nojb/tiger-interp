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

let to_be_printed = ref []

let save_for_later p =
  if not (List.mem_assq p !to_be_printed) then
    to_be_printed := (p, ref false) :: !to_be_printed

let rec code ppf = function
  | Cquote v ->
      value ppf v
  | Cget(d, i) ->
      fprintf ppf "@[<2>(%d %d)@]" d i
  | Cset(d, i, c) ->
      fprintf ppf "@[<2>(set (%d %d)@ %a)@]" d i code c
  | Cload(lnum, c1, c2) ->
      fprintf ppf "@[<2>(load@ [%d]@ %a@ %a)@]" lnum code c1 code c2
  | Cstore(lnum, c1, c2, c3) ->
      fprintf ppf "@[<2>(store@ [%d]@ %a@ %a@ %a)@]" lnum code c1 code c2 code c3
  | Cgetf(lnum, c1, i) ->
      fprintf ppf "@[<2>(getf@ [%d]@ %a@ %d)@]" lnum code c1 i
  | Csetf(lnum, c1, i, c2) ->
      fprintf ppf "@[<2>(setf@ [%d]@ %a@ %d@ %a)@]" lnum code c1 i code c2
  | Cadd(c1, c2) ->
      fprintf ppf "@[<2>(+@ %a@ %a)@]" code c1 code c2
  | Cmul(c1, c2) ->
      fprintf ppf "@[<2>(*@ %a@ %a)@]" code c1 code c2
  | Cdiv(lnum, c1, c2) ->
      fprintf ppf "@[<2>(/@ [%d]@ %a@ %a)@]" lnum code c1 code c2
  | Csub(c1, c2) ->
      fprintf ppf "@[<2>(-@ %a@ %a)@]" code c1 code c2
  | Cicmp(c1, cmp, c2)
  | Cscmp(c1, cmp, c2)
  | Cpcmp(c1, cmp, c2) ->
      fprintf ppf "@[<2>(%s@ %a@ %a)@]"
        (string_of_comparison cmp) code c1 code c2
  | Ccall(p, ca) ->
      let args ppf ca =
        Array.iter (fun a -> fprintf ppf "@ %a" code a) ca in
      save_for_later p;
      fprintf ppf "@[<2>(%s%a)@]" p.proc_name args ca
  | Cseq(c1, c2) ->
      fprintf ppf "@[<2>(seq@[<hv>@ %a@ %a@])@]" code c1 code c2
  | Cmakearray(c1, c2) ->
      fprintf ppf "@[<2>(makearray@ %a@ %a)@]" code c1 code c2
  | Cmakerecord(ca) ->
      let fields ppf ca =
        Array.iter (fun f -> fprintf ppf "@ %a" code f) ca in
      fprintf ppf "@[<2>(makerecord%a)@]" fields ca
  | Cif(c1, c2, c3) ->
      fprintf ppf "@[<2>(if@ %a@ %a@ %a)@]" code c1 code c2 code c3
  | Cwhile(c1, c2) ->
      fprintf ppf "@[<2>(while@ %a@ %a)@]" code c1 code c2
  | Cfor(d, i, c1, c2, c3) ->
      fprintf ppf "@[<2>(for (%d %d)@ %a@ %a@ %a)@]"
        d i code c1 code c2 code c3
  | Cbreak ->
      fprintf ppf "(break)"
  | Cprim(p, ca) ->
      let codes ppf args =
        Array.iter (fun a -> fprintf ppf "@ %a" code a) args in
      fprintf ppf "@[<2>(%s%a)@]"
        (string_of_primitive p) codes ca

let proc ppf p =
  fprintf ppf "@,@[<2>(define (%s %d %d)@ %a)@]@,"
    p.proc_name p.proc_frame_size p.proc_depth code p.proc_code
  (* fprintf ppf "@[; frame_size size %d\n; depth %d\n%a@]"
    p.proc_frame_size p.proc_depth code p.proc_code *)

let print_code ppf c =
  to_be_printed := [];
  fprintf ppf "@[<v>%a@," code c;
  let rec loop () =
    let ps = List.filter (fun (_, printed) -> not !printed) !to_be_printed in
    if List.length ps > 0 then
      (List.iter (fun (p, printed) -> proc ppf p; printed := true) ps; loop ()) in
  loop ();
  fprintf ppf "@,@]%!"
