open Tiger_code

let load v1 v2 =
  match v1, v2 with
  | Varray a, Vint n ->
      Printf.eprintf "load: %d\n%!" n;
      a.(n)
  | _ -> assert false

let store v1 v2 v3 =
  match v1, v2 with
  | Varray a, Vint n -> a.(n) <- v3
  | _ -> assert false

let getf v1 i =
  match v1 with
  | Vrecord None -> raise Nil
  | Vrecord (Some a) -> a.(i)
  | _ -> assert false

let setf v1 i v2 =
  match v1 with
  | Vrecord None -> raise Nil
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

let div e1 e2 =
  match e1, e2 with
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

type display =
  value array array

let get disp d i =
  disp.(d).(i)

let set disp d i v =
  disp.(d).(i) <- v

let rec eval_int disp e =
  match eval disp e with
  | Vint n -> n
  | _ -> assert false

and call disp p va =
  let old_frame = disp.(p.proc_depth) in
  let new_frame = Array.make p.proc_frame_size Vunit in
  Array.blit va 0 new_frame 0 (Array.length va);
  disp.(p.proc_depth) <- new_frame;
  let result = eval disp p.proc_body in
  disp.(p.proc_depth) <- old_frame;
  result

and eval disp = function
  | Cquote v ->
      v
  | Cget (d, i) ->
      get disp d i
  | Cset (d, i, e) ->
      set disp d i (eval disp e);
      Vunit
  | Cload (e1, e2) ->
      load (eval disp e1) (eval disp e2)
  | Cstore (e1, e2, e3) ->
      store (eval disp e1) (eval disp e2) (eval disp e3);
      Vunit
  | Cgetf (e1, i) ->
      getf (eval disp e1) i
  | Csetf (e1, i, e2) ->
      setf (eval disp e1) i (eval disp e2);
      Vunit
  | Cadd (e1, e2) ->
      add (eval disp e1) (eval disp e2)
  | Cmul (e1, e2) ->
      mul (eval disp e1) (eval disp e2)
  | Cdiv (e1, e2) ->
      div (eval disp e1) (eval disp e2)
  | Csub (e1, e2) ->
      sub (eval disp e1) (eval disp e2)
  | Cicmp (e1, cmp, e2) ->
      icmp (eval disp e1) cmp (eval disp e2)
  | Cscmp (e1, cmp, e2) ->
      scmp (eval disp e1) cmp (eval disp e2)
  | Candalso (e1, e2) ->
      if eval_int disp e1 <> 0 then eval disp e2
      else Vint 0
  | Corelse (e1, e2) ->
      let n = eval_int disp e1 in
      if n = 0 then eval disp e2 else (Vint n)
  | Ccall (p, ea) ->
      call disp p (Array.map (eval disp) ea)
  | Cseq (ea) ->
      Array.fold_left (fun _ e -> eval disp e) Vunit ea
  | Cmakearray (e1, e2) ->
      Varray (Array.make (eval_int disp e1) (eval disp e2))
  | Cmakerecord (ea) ->
      Vrecord (Some (Array.map (eval disp) ea))
  | Cif (e1, e2, e3) ->
      eval disp (if eval_int disp e1 <> 0 then e2 else e3)
  | Cwhile (e1, e2) ->
      let rec loop v1 =
        if v1 <> 0 then
          try
            ignore (eval disp e2);
            loop (eval_int disp e1)
          with Break -> Vunit
        else
          Vunit
      in loop (eval_int disp e1)
  | Cfor (d, i, e1, e2, e3) ->
      let v1 = eval_int disp e1 in
      let v2 = eval_int disp e2 in
      let rec loop u =
        if u > v2 then Vunit
        else begin
          set disp d i (Vint u);
          try
            ignore (eval disp e3);
            loop (u + 1)
          with Break -> Vunit
        end
      in loop v1
  | Cbreak ->
      raise Break
  | Cprim (f, ea) ->
      f (Array.map (eval disp) ea)

let run (max_static_depth, frame_size, e) =
  let disp = Array.make (max_static_depth + 1) [| |] in
  disp.(0) <- Array.make frame_size Vunit;
  eval disp e
