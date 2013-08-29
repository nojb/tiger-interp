let compile inp =
  Compile.transl_program
    (Parser.program Lexer.token
      (Lexing.from_channel inp))

let compile_and_run input =
  Eval.run (compile input)

let compile_and_run_file filename =
  try
    print_endline (Code.string_of_value (compile_and_run (open_in filename)))
  with
    Error.Error(p, msg) ->
      Printf.eprintf "%s: %d: %d: %s.\n%!"
        p.Lexing.pos_fname
        p.Lexing.pos_lnum (p.Lexing.pos_cnum - p.Lexing.pos_bol + 1) msg

let _ =
  Arg.parse [] compile_and_run_file ""
