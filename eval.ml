open Code

type runtime_error =
  | Out_of_bounds
  | Nil_record
  | Div_by_zero

exception Runtime_error of int * runtime_error
exception Break
exception Exit of int

let eval_primitive p va =
  match p, va with
  | Pprint, [| Vstring s |] ->
      print_string s;
      Vunit
  | Pprinti, [| Vint i |] ->
      print_int i;
      Vunit
  | Pflush, [| |] ->
      flush stdout;
      Vunit
  | Pgetchar, [| |] ->
      Vstring (try String.make 1 (input_char stdin) with End_of_file -> "")
  | Pord, [| Vstring "" |] ->
      Vint (-1)
  | Pord, [| Vstring s |] ->
      Vint (int_of_char s.[0])
  | Pchr, [| Vint i |] ->
      Vstring (String.make 1 (char_of_int i))
  | Psize, [| Vstring s |] ->
      Vint (String.length s)
  | Psubstring, [| Vstring s; Vint f; Vint n |] ->
      Vstring (String.sub s f n)
  | Pconcat, [| Vstring s1; Vstring s2 |] ->
      Vstring (s1 ^ s2)
  | Pnot, [| Vint 0 |] ->
      Vint 1
  | Pnot, [| Vint _ |] ->
      Vint 0
  | Pexit, [| Vint i |] ->
      raise (Exit i)
  | Psizea, [| Varray a |] ->
      Vint (Array.length a)
  | _ -> assert false

let load lnum v1 v2 =
  match v1, v2 with
  | Varray a, Vint n ->
      if n >= Array.length a || n < 0 then
        raise (Runtime_error(lnum, Out_of_bounds))
      else
        a.(n)
  | _ -> assert false

let store lnum v1 v2 v3 =
  match v1, v2 with
  | Varray a, Vint n ->
      if n >= Array.length a || n < 0 then
        raise (Runtime_error(lnum, Out_of_bounds))
      else
        a.(n) <- v3
  | _ -> assert false

let getf lnum v1 i =
  match v1 with
  | Vrecord None -> raise (Runtime_error(lnum, Nil_record))
  | Vrecord (Some a) -> a.(i)
  | _ -> assert false

let setf lnum v1 i v2 =
  match v1 with
  | Vrecord None -> raise (Runtime_error(lnum, Nil_record))
  | Vrecord (Some a) -> a.(i) <- v2
  | _ -> assert false

let add e1 e2 =
  match e1, e2 with
  | Vint n, Vint m -> Vint (n + m)
  | _ -> assert false

let mul e1 e2 =
  match e1, e2 with
  | Vint n, Vint m -> Vint (n * m)
  | _ -> assert false

let div lnum e1 e2 =
  match e1, e2 with
  | Vint _, Vint 0 -> raise (Runtime_error(lnum, Div_by_zero))
  | Vint n, Vint m -> Vint (n / m)
  | _ -> assert false

let sub e1 e2 =
  match e1, e2 with
  | Vint n, Vint m -> Vint (n - m)
  | _ -> assert false

let icmp v1 cmp v2 =
  match v1, cmp, v2 with
  | Vint n, Ceq, Vint m -> Vint (if n  = m then 1 else 0)
  | Vint n, Cne, Vint m -> Vint (if n <> m then 1 else 0)
  | Vint n, Clt, Vint m -> Vint (if n  < m then 1 else 0)
  | Vint n, Cle, Vint m -> Vint (if n <= m then 1 else 0)
  | Vint n, Cge, Vint m -> Vint (if n >= m then 1 else 0)
  | Vint n, Cgt, Vint m -> Vint (if n  > m then 1 else 0)
  | _ -> assert false

let scmp v1 cmp v2 =
  match v1, cmp, v2 with
  | Vstring s1, Ceq, Vstring s2 -> Vint (if s1  = s2 then 1 else 0)
  | Vstring s1, Cne, Vstring s2 -> Vint (if s1 <> s2 then 1 else 0)
  | Vstring s1, Clt, Vstring s2 -> Vint (if s1  < s2 then 1 else 0)
  | Vstring s1, Cle, Vstring s2 -> Vint (if s1 <= s2 then 1 else 0)
  | Vstring s1, Cge, Vstring s2 -> Vint (if s1 >= s2 then 1 else 0)
  | Vstring s1, Cgt, Vstring s2 -> Vint (if s1  > s2 then 1 else 0)
  | _ -> assert false

let rec eval_int disp e =
  match eval disp e with
  | Vint n -> n
  | _ -> assert false

and call disp p va =
  let old_frame = disp.(p.proc_depth) in
  let new_frame = Array.make p.proc_frame_size Vunit in
  Array.blit va 0 new_frame 0 (Array.length va);
  disp.(p.proc_depth) <- new_frame;
  let result = eval disp p.proc_code in
  disp.(p.proc_depth) <- old_frame;
  result

and eval disp = function
  | Cquote v ->
      v
  | Cget(d, i) ->
      disp.(d).(i)
  | Cset(d, i, e) ->
      disp.(d).(i) <- (eval disp e);
      Vunit
  | Cload(lnum, e1, e2) ->
      load lnum (eval disp e1) (eval disp e2)
  | Cstore(lnum, e1, e2, e3) ->
      store lnum (eval disp e1) (eval disp e2) (eval disp e3);
      Vunit
  | Cgetf(lnum, e1, i) ->
      getf lnum (eval disp e1) i
  | Csetf(lnum, e1, i, e2) ->
      setf lnum (eval disp e1) i (eval disp e2);
      Vunit
  | Cadd(e1, e2) ->
      add (eval disp e1) (eval disp e2)
  | Cmul(e1, e2) ->
      mul (eval disp e1) (eval disp e2)
  | Cdiv(lnum, e1, e2) ->
      div lnum (eval disp e1) (eval disp e2)
  | Csub(e1, e2) ->
      sub (eval disp e1) (eval disp e2)
  | Cicmp(e1, cmp, e2) ->
      icmp (eval disp e1) cmp (eval disp e2)
  | Cscmp(e1, cmp, e2) ->
      scmp (eval disp e1) cmp (eval disp e2)
  | Candalso(e1, e2) ->
      if eval_int disp e1 <> 0 then eval disp e2
      else Vint 0
  | Corelse(e1, e2) ->
      let n = eval_int disp e1 in
      if n = 0 then eval disp e2 else (Vint n)
  | Ccall(p, ea) ->
      call disp p (Array.map (eval disp) ea)
  | Cseq(e1, e2) ->
      ignore (eval disp e1);
      eval disp e2
  | Cmakearray(e1, e2) ->
      Varray (Array.make (eval_int disp e1) (eval disp e2))
  | Cmakerecord(ea) ->
      Vrecord (Some (Array.map (eval disp) ea))
  | Cif(e1, e2, e3) ->
      eval disp (if eval_int disp e1 <> 0 then e2 else e3)
  | Cwhile(e1, e2) ->
      let rec loop v1 =
        if v1 <> 0 then
          try
            ignore (eval disp e2);
            loop (eval_int disp e1)
          with Break -> Vunit
        else
          Vunit
      in loop (eval_int disp e1)
  | Cfor(d, i, e1, e2, e3) ->
      let v1 = eval_int disp e1 in
      let v2 = eval_int disp e2 in
      let rec loop u =
        if u > v2 then Vunit
        else begin
          disp.(d).(i) <- Vint u;
          try
            ignore (eval disp e3);
            loop (u + 1)
          with Break -> Vunit
        end
      in loop v1
  | Cbreak ->
      raise Break
  | Cprim(p, ea) ->
      eval_primitive p (Array.map (eval disp) ea)

let run (max_static_depth, frame_size, e) =
  let disp = Array.make (max_static_depth + 1) [| |] in
  disp.(0) <- Array.make frame_size Vunit;
  eval disp e
