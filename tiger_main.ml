let compile inp =
  Tiger_compile.transl_program
    (Tiger_parser.program Tiger_lexer.token
      (Lexing.from_channel inp))

let compile_and_run input =
  Tiger_eval.run (compile input)

let compile_and_run_file filename =
  compile_and_run (open_in filename)

let _ =
  while true do
    print_string "> ";
    flush stdout;
    print_endline (Tiger_code.string_of_value (compile_and_run stdin));
  done
