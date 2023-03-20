open Base

type address = 
  | Address of int
  | LabelRef of string 
  | RegisterRef of string
[@@deriving compare, equal, sexp]

type reg = 
  | M 
  | D 
  | A
[@@deriving compare, equal, sexp] 

type operand = 
  | Reg of reg 
  | One
  | Zero 
[@@deriving compare, equal, sexp] 

type dest = Dest of reg * (reg option)

type jump = 
  | NoJump 
  | Jgt 
  | Jeq 
  | Jge
  | Jlt 
  | Jne 
  | Jle
  | Jmp
[@@deriving compare, equal, sexp]

type operation = 
  | Eq of operand * operand
  | Minus of operand * operand
  | Plus of operand * operand
  | Or of operand * operand 
  | And of operand * operand
  | Not of operand 
  | Selection of operand 
  | Assignment of reg * operation
[@@deriving compare, equal, sexp]

type destination = 
  | NoDest
  | DestOne of reg 
  | DestTwo of reg * reg 
  | DestThree of reg * reg * reg
[@@deriving compare, equal, sexp]

type inst =
  | Atom of string * int 
  | Address of int
  | AddressRef of string 
  | Label of string * int
  | Program of inst list
  | AInstruction of address
  | CInstruction of destination * operation * jump
  | List of inst list
[@@deriving compare, equal, sexp]


