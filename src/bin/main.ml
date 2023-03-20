open Base
open Stdio

let printf = Stdlib.Printf.printf

let parse_and_build_symbol_table = 
  (* enable pretty error messages *)
  Hackasm.Parser.pp_exceptions ();
  let prog = Hackasm.Parser.parse_chan stdin in
  Hackasm.Symbol_table.make_from_program prog |> fst

let () = begin

  (* enable pretty error messages *)
  Hackasm.Parser.pp_exceptions ();

  let prog = parse_and_build_symbol_table in

  (*
  prog
  |> Hackasm.Ast.sexp_of_inst
  |> Sexp.to_string_hum
  |> printf "-> %s\n";
  *)

  prog
  |> Hackasm.Codegen.assemble_top_level
  |> List.iter ~f:print_endline;


  (*|> Hackasm.Ast.sexp_of_inst
    |> Sexp.to_string_hum
    |> printf "-> %s\n";

    match (Hackasm.Symbol_table.lookup "RET_ADDRESS_LT4") with
    | Some(a) -> printf "%d" a
    | None -> printf "Not found"*)

  (*
  List.iter [l1] ~f:(fun s ->
      Hackasm.Parser.parse_string (String.strip s)
      |> Hackasm.Codegen.assemble_top_level
      |> List.iter ~f:print_endline
    );
    *)
end
