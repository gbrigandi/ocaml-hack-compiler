open Base

type t

val empty: t

val lookup : string -> t -> int

val add : string -> int -> t -> t

val make_from_program : Ast.inst -> Ast.inst * t
