include Nice_parser.Make(struct
    type result = Ast.inst
    type token = Menhir_parser.token
    exception ParseError = Menhir_parser.Error
    let parse = Menhir_parser.inst_eof
    include Lexer
  end)

let%test_module _ = (module struct

  let () = Printexc.record_backtrace false

  let%expect_test "label" =
    parse_string "(this_is_A_Label)"
    |> Printf.printf !"%{sexp:Ast.inst}";
    [%expect{| (Program ((Label this_is_A_Label 1))) |}]

  let%expect_test "multiple labels" =
    parse_string "(label_one)\n(label_two)"
    |> Printf.printf !"%{sexp:Ast.inst}";
    [%expect{| (Program ((Label label_one 1) (Label label_two 2))) |}]

  let%expect_test "A instruction" =
    parse_string "@1"
    |> Printf.printf !"%{sexp:Ast.inst}";
    [%expect{| (Program ((AInstruction (Address 1)))) |}]

  let%expect_test "C instruction" =
    parse_string "M=D+1;JGE"
    |> Printf.printf !"%{sexp:Ast.inst}";
    [%expect{| (Program ((CInstruction (DestOne M) (Plus (Reg D) One) Jge))) |}]

  let%expect_test "illegal atom" =
    parse_string "(  *$-# )"
    |> Printf.printf !"%{sexp:Ast.inst}";  
    [%expect.unreachable]
  [@@expect.uncaught_exn {|
    ("Nice_parser.Make(P).LexError(\"[lexer] unexpected character: '*'\", _)")
  |}]

end)
