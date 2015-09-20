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

external caml_string_get32 : string -> int -> int32 = "%caml_string_get32"
external caml_string_get64 : bytes -> int -> int64 = "%caml_string_get64"
external caml_string_set64 : bytes -> int -> int64 -> unit = "%caml_string_set64"

let get32 s i = Int32.to_int (caml_string_get32 s i)
let get64 s i = Int64.to_int (caml_string_get64 s i)
let set64 s i n = caml_string_set64 s i (Int64.of_int n)

type opcode =
  | ACC
  | PUSH
  | POP
  | ASSIGN
  | GETFIELD
  | SETFIELD
  | BRANCH
  | BRANCHIF
  | BOOLNOT
  | C_CALL
  | CONST0
  | NEGINT
  | ADDINT
  | SUBINT
  | MULINT
  | DIVINT
  | PUSH_RETADDR
  | RETURN

let decode_opcode code pc : opcode =
  Obj.magic (get32 code pc)

type primitive =
  | Primitive1 of (int -> int)
  | Primitive2 of (int -> int -> int)
  | Primitive3 of (int -> int -> int -> int)
  | Primitive4 of (int -> int -> int -> int -> int)

let primitive _ = assert false

let run mem code ~pc =
  let rec step ~pc ~sp ~hp ~accu =
    let i = decode_opcode code pc in
    let pc = pc + 4 in
    match i with
    | ACC ->
        let accu = get64 mem (sp + get32 code pc lsl 3) in
        let pc = pc + 4 in
        step ~pc ~sp ~hp ~accu
    | PUSH ->
        let sp = sp - 8 in
        set64 mem sp accu;
        step ~pc ~sp ~hp ~accu
    | POP ->
        let sp = sp + get32 code pc lsl 3 in
        let pc = pc + 4 in
        step ~pc ~sp ~hp ~accu
    | ASSIGN ->
        set64 mem (sp + get32 code pc lsl 3) accu;
        let pc = pc + 4 in
        let accu = 0 in
        step ~pc ~sp ~hp ~accu
    | GETFIELD ->
        let accu = get64 mem (accu + get32 code pc lsl 3) in
        let pc = pc + 4 in
        step ~pc ~sp ~hp ~accu
    | SETFIELD ->
        set64 mem (accu + get32 code pc lsl 3) (get64 mem sp);
        let sp = sp + 8 in
        let accu = 0 in
        step ~pc ~sp ~hp ~accu
    | BRANCH ->
        let pc = pc + get32 code pc in
        step ~pc ~sp ~hp ~accu
    | BRANCHIF ->
        let pc = if accu = 0 then pc + 4 else pc + get32 code pc in
        step ~pc ~sp ~hp ~accu
    | BOOLNOT ->
        let accu = if accu = 0 then 1 else 0 in
        step ~pc ~sp ~hp ~accu
    | C_CALL ->
        let accu, sp = match primitive (get32 code pc) with
          | Primitive1 f ->
              f accu, sp
          | Primitive2 f ->
              f accu (get64 mem sp), sp + 8
          | Primitive3 f ->
              f accu (get64 mem sp) (get64 mem (sp + 8)), sp + 16
          | Primitive4 f ->
              f accu (get64 mem sp) (get64 mem (sp + 8)) (get64 mem (sp + 16)), sp + 24
        in
        step ~pc ~sp ~hp ~accu
    | CONST0 ->
        let accu = 0 in
        step ~pc ~sp ~hp ~accu
    | NEGINT ->
        let accu = -accu in
        step ~pc ~sp ~hp ~accu
    | ADDINT ->
        let accu = accu + get64 mem sp in
        let sp = sp + 8 in
        step ~pc ~sp ~hp ~accu
    | SUBINT ->
        let accu = accu - get64 mem sp in
        let sp = sp + 8 in
        step ~pc ~sp ~hp ~accu
    | MULINT ->
        let accu = accu * get64 mem sp in
        let sp = sp + 8 in
        step ~pc ~sp ~hp ~accu
    | DIVINT ->
        let accu = accu / get64 mem sp in
        let sp = sp + 8 in
        step ~pc ~sp ~hp ~accu
    | PUSH_RETADDR ->
        let sp = sp - 8 in
        set64 mem sp (get32 code pc);
        let pc = pc + 4 in
        step ~pc ~sp ~hp ~accu
    | RETURN ->
        let sp = sp + get32 code pc lsl 3 in
        let pc = get64 mem sp in
        let sp = sp + 8 in
        step ~pc ~sp ~hp ~accu
  in
  let hp = 0 in
  let sp = Bytes.length mem in
  let accu = 0 in
  step ~pc ~sp ~hp ~accu
