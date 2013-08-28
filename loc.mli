type t

val dummy : t
val make : Lexing.position -> Lexing.position -> t
val pp : Format.formatter -> t -> unit
