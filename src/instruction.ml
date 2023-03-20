open Base
open Binary
open Ast

let comp_inst = 
  [ 
    Ast.Selection Ast.Zero, 0b0101010;
    Ast.Selection Ast.One, 0b0111111;
    Ast.Minus (Ast.Zero, Ast.One), 0b0111010;
    Ast.Selection (Ast.Reg Ast.D), 0b0001100;
    Ast.Selection (Ast.Reg Ast.A), 0b0110000;
    Ast.Selection (Ast.Reg Ast.M), 0b1110000;
    Ast.Not (Ast.Reg Ast.D),       0b0001101; 
    Ast.Not (Ast.Reg Ast.A),       0b0110001; 
    Ast.Not (Ast.Reg Ast.M),       0b1110001; 
    Ast.Minus (Ast.Zero, Ast.Reg Ast.D), 0b0001111;
    Ast.Minus (Ast.Zero, Ast.Reg Ast.A), 0b0110011;
    Ast.Minus (Ast.Zero, Ast.Reg Ast.M), 0b1110011;
    Ast.Plus (Ast.Reg Ast.D, Ast.One), 0b0011111;
    Ast.Plus (Ast.Reg Ast.A, Ast.One), 0b0110111;
    Ast.Plus (Ast.Reg Ast.M, Ast.One), 0b1110111;
    Ast.Minus (Ast.Reg Ast.D, Ast.One), 0b0001110;
    Ast.Minus (Ast.Reg Ast.A, Ast.One), 0b0110010;
    Ast.Minus (Ast.Reg Ast.M, Ast.One), 0b1110010;
    Ast.Plus (Ast.Reg Ast.D, Ast.Reg Ast.A), 0b0000010;
    Ast.Plus (Ast.Reg Ast.D, Ast.Reg Ast.M), 0b1000010;
    Ast.Minus (Ast.Reg Ast.D, Ast.Reg Ast.A), 0b0010011;
    Ast.Minus (Ast.Reg Ast.D, Ast.Reg Ast.M), 0b1010011;
    Ast.Minus (Ast.Reg Ast.A, Ast.Reg Ast.D), 0b0000111;
    Ast.Minus (Ast.Reg Ast.M, Ast.Reg Ast.D), 0b1000111;
    Ast.And (Ast.Reg Ast.D, Ast.Reg Ast.A), 0b0000000;
    Ast.And (Ast.Reg Ast.D, Ast.Reg Ast.M), 0b1000000;
    Ast.Or (Ast.Reg Ast.D, Ast.Reg Ast.A), 0b0010101;
    Ast.Or (Ast.Reg Ast.D, Ast.Reg Ast.M), 0b1010101;
  ]

let dest_inst = 
  [
    Ast.NoDest, 0b000;
    Ast.DestOne (Ast.M), 0b001;
    Ast.DestOne (Ast.D), 0b010;
    Ast.DestTwo (Ast.M, Ast.D), 0b011;
    Ast.DestOne (Ast.A), 0b100;
    Ast.DestTwo (Ast.A, Ast.M), 0b101;
    Ast.DestTwo (Ast.A, Ast.D), 0b110;
    Ast.DestThree (Ast.A, Ast.M, Ast.D), 0b111;  
  ]

let jmp_inst = 
  [
    Ast.NoJump, 0b000;
    Ast.Jgt,  0b001;
    Ast.Jeq,  0b010;
    Ast.Jge,  0b011;
    Ast.Jlt,  0b011;
    Ast.Jne,  0b101;
    Ast.Jle,  0b110;
    Ast.Jmp,  0b111;
  ]

let make_a_instruction addr =
  int_to_bits 16 addr
  |> bits_to_bitstring

let make_c_instruction dest comp jmp = 
  let p = 0b111 in
  let c = List.Assoc.find_exn ~equal:equal_operation comp_inst comp in
  let d = List.Assoc.find_exn ~equal:equal_destination dest_inst dest in
  let j = List.Assoc.find_exn ~equal:equal_jump jmp_inst jmp in
  (p lsl 13) lor (c lsl 6) lor (d lsl 3) lor j 
  |> int_to_bits 16
  |> bits_to_bitstring

let%test _ = String.equal (make_a_instruction 0xffff) "1111111111111111"

let%test _ = String.equal (make_c_instruction (Ast.DestOne (Ast.M)) (Ast.Plus (Ast.Reg Ast.D, Ast.One)) Ast.NoJump) "1110011111001000"
