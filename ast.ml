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

type pos =
  Lexing.position

type ident = {
  id : string;
  pos : pos
}

type cmp =
  | Eq | Le | Ge | Lt | Gt | Ne

type bin =
  | Op_add | Op_sub | Op_mul | Op_div
  | Ocmp of cmp

type typ =
  | Tname     of ident
  | Tarray    of ident
  | Trecord   of (ident * ident) list

type var =
  | Vsimple     of ident
  | Vsubscript  of pos * var * exp
  | Vfield      of pos * var * ident

and exp =
  | Eunit     of pos
  | Eint      of pos * int
  | Estring   of pos * string
  | Enil      of pos
  | Evar      of var
  | Ebinop    of pos * exp * bin * exp
  | Eassign   of pos * var * exp
  | Ecall     of pos * ident * exp list
  | Eseq      of exp * exp
  | Earray    of pos * ident * exp * exp
  | Erecord   of pos * ident * (ident * exp) list
  | Eif       of pos * exp * exp * exp
  | Ewhile    of pos * exp * exp
  | Efor      of pos * ident * exp * exp * exp
  | Ebreak    of pos
  | Elet      of pos * dec * exp

and dec =
  | Dtypes  of (ident * typ) list
  | Dfuns   of fundec list
  | Dvar    of ident * ident option * exp

and fundec = {
  fun_name : ident;
  fun_args : (ident * ident) list;
  fun_rety : ident option;
  fun_body : exp
}

let rec var_pos = function
  | Vsimple id -> id.pos
  | Vsubscript(p, _, _) | Vfield(p, _, _) -> p

and exp_pos = function
  | Eunit p | Eint(p, _) | Estring(p, _) | Enil p -> p
  | Evar v -> var_pos v
  | Ebinop(p, _, _, _) | Eassign(p, _, _) | Ecall(p, _, _) -> p
  | Eseq(e, _) -> exp_pos e
  | Earray(p, _, _, _) | Erecord(p, _, _)
  | Eif(p, _, _, _) | Ewhile(p, _, _) | Efor(p, _, _, _, _) -> p
  | Ebreak p | Elet(p, _, _) -> p
