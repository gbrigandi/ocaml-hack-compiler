open Base
open Ast
open Instruction

let assemble_top_level p = 
  let assemble i =
    match i with
    | AInstruction (Address addr)  -> Some (make_a_instruction addr)
    | CInstruction (dest, comp, jmp) -> Some (make_c_instruction dest comp jmp)
    | _ -> None
      
  in    
  match p with
  | Program insts ->
    List.filter_map insts ~f:assemble
  | _ -> []
    

