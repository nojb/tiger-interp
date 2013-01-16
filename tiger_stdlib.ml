open Tiger_code

let tiger_print = function
  | [| Vstring s |] -> print_string s; Vunit
  | _ -> assert false

let tiger_printi = function
  | [| Vint i |] -> print_int i; Vunit
  | _ -> assert false
  
let tiger_flush = function
  | [| |] -> flush stdout; Vunit
  | _ -> assert false

let tiger_getchar = function
  | [| |] ->
      Vstring (try String.make 1 (input_char stdin) with End_of_file -> "")
  | _ -> assert false

let tiger_ord = function
  | [| Vstring "" |] -> Vint (-1)
  | [| Vstring s |] -> Vint (int_of_char s.[0])
  | _ -> assert false

let tiger_chr = function
  | [| Vint i |] -> Vstring (String.make 1 (char_of_int i))
  | _ -> assert false

let tiger_size = function
  | [| Vstring s |] -> Vint (String.length s)
  | _ -> assert false

let tiger_substring = function
  | [| Vstring s; Vint f; Vint n |] -> Vstring (String.sub s f n)
  | _ -> assert false

let tiger_concat = function
  | [| Vstring s1; Vstring s2 |] -> Vstring (s1 ^ s2)
  | _ -> assert false

let tiger_not = function
  | [| Vint 0 |] -> Vint 1
  | [| Vint _ |] -> Vint 0
  | _ -> assert false

let tiger_exit = function
  | [| Vint i |] -> exit i
  | _ -> assert false
