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

type value =
  | Vunit
  | Vint of int
  | Vstring of string
  | Varray of value array
  | Vrecord of value array option

type comparison =
  | Ceq | Cle | Clt | Cne | Cge | Cgt

type primitive =
  | Pprint
  | Pprinti
  | Pflush
  | Pgetchar
  | Pord
  | Pchr
  | Psize
  | Psubstring
  | Pconcat
  | Pnot
  | Pexit

type code =
  | Cquote of value
  | Cvar of string
  | Cassign of string * code
  | Cload of int * code * code
  | Cstore of int * code * code * code
  | Cgetf of int * code * int
  | Csetf of int * code * int * code
  | Cadd of code * code
  | Cmul of code * code
  | Cdiv of int * code * code
  | Csub of code * code
  | Cicmp of code * comparison * code
  | Cscmp of code * comparison * code
  | Cpcmp of code * comparison * code
  | Ccall of string * code list
  | Cseq of code * code
  | Cmakearray of code * code
  | Cmakerecord of code list
  | Cif of code * code * code
  | Cwhile of code * code
  | Cfor of string * code * code * code
  | Cbreak
  | Cprim of primitive * code list
  | Clet of string * code * code
  | Cletrec of (string * string list * code) list * code

let rec string_of_value = function (* loops if circular data type *)
  | Vunit -> ""
  | Vint n -> string_of_int n
  | Vstring s -> s
  | Varray a ->
      "[" ^ (String.concat "; " (Array.to_list (Array.map string_of_value a))) ^ "]"
  | Vrecord None ->
      "nil"
  | Vrecord (Some a) ->
      "{" ^ (String.concat "; " (Array.to_list (Array.map string_of_value a))) ^ "}"
