open Tiger_types
open Tiger_code

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
  mutable env_bindings : symbol_info M.t
}

let create () = {
  env_frame_size = ref 0;
  env_depth = 0;
  env_max_depth = ref 0;
  env_bindings = M.empty
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
    M.add name (Svariable {var_type = ty; var_depth = env.env_depth;
      var_frame_index = idx}) env.env_bindings;
  env.env_depth, idx

let find_variable env name =
  match M.find name env.env_bindings with
  | Svariable v -> v
  | Sfunction _ -> raise Not_found

let add_function env name sg =
  let dummy_desc = { proc_frame_size = 0; proc_code = Cquote Vunit; proc_depth =
    env.env_depth + 1; proc_name = name } in
  env.env_bindings <-
    M.add name (Sfunction {fn_signature = sg; fn_desc = User dummy_desc}) env.env_bindings

let add_primitive env name sg p =
  env.env_bindings <-
    M.add name (Sfunction {fn_signature = sg; fn_desc = Builtin p}) env.env_bindings

let find_function env name =
  match M.find name env.env_bindings with
  | Svariable _ -> raise Not_found
  | Sfunction f -> f

let max_static_depth env =
  !(env.env_max_depth)

let frame_size env =
  !(env.env_frame_size)
