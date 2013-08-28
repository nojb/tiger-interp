let compile inp =
  Compile.transl_program
    (Parser.program Lexer.token
      (Lexing.from_channel inp))

let compile_and_run input =
  Eval.run (compile input)

let compile_and_run_file filename =
  print_endline (Code.string_of_value (compile_and_run (open_in filename)))

let _ =
  Arg.parse [] compile_and_run_file ""
