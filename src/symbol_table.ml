
open Base

module StringMap = Stdlib.Map.Make(String)

type t = (int StringMap.t)

let empty = StringMap.empty

let lookup s t = StringMap.find s t

let add s l t = StringMap.add s l t

let make_from_program p =
  let rec process_label_symbols t ins idx o = 
    match ins with
    | [] -> (o, t)
    | x :: xs ->
      match x with 
      | Ast.Label(n, _) ->
        (match (StringMap.find_opt n t) with
         | Some _ -> (o, t)   
         | None -> process_label_symbols (add n idx t) xs idx (Ast.Label(n, idx) :: o)
        )
      | _ -> process_label_symbols t xs (idx+1) (x :: o)
  in 
  let rec process_ainstruction_symbols t idx o ins = 
    match ins with
    | [] -> (o, t)
    | x :: xs ->
      match x with 
      | Ast.AInstruction(Ast.LabelRef l) ->
        (match (StringMap.find_opt l t) with
         | Some y  ->
           process_ainstruction_symbols t idx (Ast.AInstruction (Address y) :: o ) xs   
         | None -> process_ainstruction_symbols (add l (16 + idx) t) (idx+1) (Ast.AInstruction(Ast.Address(16 + idx)) :: o) xs
        )
      | Ast.AInstruction(Ast.RegisterRef r) ->
        let rint = String.sub r ~pos:1 ~len:((String.length r) -1 ) |> Int.of_string  in 
        process_ainstruction_symbols t idx (Ast.AInstruction(Ast.Address rint) :: o) xs
      | _ -> process_ainstruction_symbols t idx (x :: o) xs  

  in 
  match p with
  | (Ast.Program instructions) ->
    let symbols = empty in
    let (ls, sta) = process_label_symbols symbols instructions 0 [] in
    let (ains, stb) = process_ainstruction_symbols sta 0 [] (List.rev ls) in
    (Ast.Program (List.rev ains), stb)
  | _ -> (Ast.Program ([]), empty) 

let%test "add symbol" =
  let t1 = add "this_is_A_Label" 1 empty  in
  let t2 = add "this_is_A_Label_2" 2 t1 in
  lookup "this_is_A_Label" t2 = 1 

let%test "lookup symbol" =
  let t1 = add "this_is_A_Label_1" 1 empty in
  lookup "this_is_A_Label_1" t1 = 1 

