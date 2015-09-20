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

type variable_info = {
  var_depth : int;
  var_frame_index : int;
  var_type : tiger_type
}

type function_desc =
  | User of proc
  | Builtin of primitive

type function_info = {
  fn_desc : function_desc;
  fn_signature : tiger_type list * tiger_type
}

type symbol_info =
  | Svariable of variable_info
  | Sfunction of function_info

type env = {
  env_frame_size : int ref;
  env_depth : int;
  env_max_depth : int ref;
  mutable env_bindings : symbol_info StringMap.t
}

let create () = {
  env_frame_size = ref 0;
  env_depth = 0;
  env_max_depth = ref 0;
  env_bindings = StringMap.empty
}

let new_frame env =
  let env = {
    env with
      env_frame_size = ref 0;
      env_depth = env.env_depth + 1 } in
  env.env_max_depth := max !(env.env_max_depth) env.env_depth;
  env

let new_scope env = {
  env_frame_size = env.env_frame_size;
  env_depth = env.env_depth;
  env_max_depth = env.env_max_depth;
  env_bindings = env.env_bindings
}

let add_variable env name ty =
  let idx = !(env.env_frame_size) in
  incr env.env_frame_size;
  env.env_bindings <-
    StringMap.add name (Svariable {var_type = ty; var_depth = env.env_depth;
      var_frame_index = idx}) env.env_bindings;
  env.env_depth, idx

let find_variable env name =
  match StringMap.find name env.env_bindings with
  | Svariable v -> v
  | Sfunction _ -> raise Not_found

let add_function env name sg =
  let dummy_desc = { proc_frame_size = 0; proc_code = Cquote Vunit; proc_depth =
    env.env_depth + 1; proc_name = name } in
  env.env_bindings <-
    StringMap.add name (Sfunction {fn_signature = sg; fn_desc = User dummy_desc}) env.env_bindings

let add_primitive env name sg p =
  env.env_bindings <-
    StringMap.add name (Sfunction {fn_signature = sg; fn_desc = Builtin p}) env.env_bindings

let find_function env name =
  match StringMap.find name env.env_bindings with
  | Svariable _ -> raise Not_found
  | Sfunction f -> f

let max_static_depth env =
  !(env.env_max_depth)

let frame_size env =
  !(env.env_frame_size)
