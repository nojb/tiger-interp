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

type tiger_type =
  | TIGvoid
  | TIGint
  | TIGstring
  | TIGarray of string * tiger_type
  | TIGrecord of string * (string * tiger_type) list
  | TIGnamed of string

module StringMap = Map.Make(String)

let rec unroll tenv t =
  match t with
  | TIGnamed id -> unroll tenv (StringMap.find id tenv)
  | TIGvoid | TIGint | TIGstring | TIGarray _
  | TIGrecord _ -> t

let type_equal tenv t1 t2 =
  unroll tenv t1 == unroll tenv t2

let rec describe_type = function
  | TIGvoid -> "void"
  | TIGint -> "int"
  | TIGstring -> "string"
  | TIGarray(id, t) -> Printf.sprintf "%s = array of %s" id (describe_type t)
  | TIGrecord(id, xts) ->
      Printf.sprintf "%s = {%s}" id
        (String.concat ", "
          (List.map (fun (x, t) -> x ^ " : " ^ describe_type t) xts))
  | TIGnamed id -> id

let base_tenv =
  StringMap.add "int" TIGint
    (StringMap.add "string" TIGstring StringMap.empty)
