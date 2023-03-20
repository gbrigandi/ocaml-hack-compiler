open Base
open List

let int_to_bits s v =
  let rec to_bits s v o =
    if v = 0
    then
      String.to_list (String.make (s - List.length o) '0') @ o
    else 
      let nv = v / 2 in
      let r = Int.rem v 2 in
      if r = 0 then (to_bits [@tailcall]) s nv ('0'::o) else (to_bits [@tailcall]) s nv ('1'::o)  
  in 
  to_bits s v []  

let bits_to_bitstring b = 
  fold b ~init:[""] ~f:(fun x a -> x @ [String.make 1 a])
  |> String.concat  

let bitstring_to_int b = 
  let rec btoi v p o =
    if p = 0 then o else (btoi [@tailcall]) v (p-1) (o + (Char.get_digit_exn @@ String.get v (p-1)) * (2 ** (p-1))) 
  in
  btoi (String.rev b) (String.length b) 0
