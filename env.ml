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

module StringMap = Map.Make(String)

type function_desc =
  | User
  | Builtin of primitive

type function_info =
  {
    fn_desc : function_desc;
    fn_signature : tiger_type list * tiger_type
  }

type symbol_info =
  | Svariable of tiger_type
  | Sfunction of function_info

type env = symbol_info StringMap.t

let empty = StringMap.empty

let add_variable env name ty =
  StringMap.add name (Svariable ty) env

let find_variable env name =
  match StringMap.find name env with
  | Svariable v -> v
  | Sfunction _ -> raise Not_found

let add_function env name typs rty =
  StringMap.add name (Sfunction {fn_signature = (typs, rty); fn_desc = User}) env

let add_primitive env name typs rty p =
  StringMap.add name (Sfunction {fn_signature = (typs, rty); fn_desc = Builtin p}) env

let find_function env name =
  match StringMap.find name env with
  | Svariable _ -> raise Not_found
  | Sfunction f -> f
