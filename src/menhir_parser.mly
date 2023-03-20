(* tokens *)
%token ONE ZERO MINUS LPAR RPAR SEMICOLON EXCL AND OR AT NL EOF D M A MD AMD AM AD EQ PLUS SP LCL ARG NULL THIS THAT SCREEN KBD JGT JEQ JGE JLT JNE JLE JMP   

%token <string> REGISTER
%token <string * int> ATOM
%token <int> NUMBER

(* start symbol *)
%start <Ast.inst> inst_eof

%%

inst_eof:
  | e=program; EOF { e }
  ;

program:
  | i = instructions; { Ast.Program i }
  ;

instructions:
    | i = separated_nonempty_list(NL, instruction) { i };
    ;

instruction:    
    | LPAR; a=ATOM; RPAR; { Ast.Label (fst a, snd a) }
    | AT; a=address; { Ast.AInstruction a }
    | d=destination; EQ; c=compute; SEMICOLON; j=jump { Ast.CInstruction (d, c, j) }
    | d=destination; EQ; c=compute; { Ast.CInstruction (d, c, Ast.NoJump) }
    | c=compute; {Ast.CInstruction (Ast.NoDest, c, Ast.NoJump )}
    | c=compute; SEMICOLON; j=jump { Ast.CInstruction (Ast.NoDest, c, j )}
    ;

destination:
    | AMD { Ast.DestThree (Ast.A, Ast.M, Ast.D) }
    | AM  { Ast.DestTwo (Ast.A, Ast.M) }
    | AD  { Ast.DestTwo (Ast.A, Ast.D) }
    | MD { Ast.DestTwo (Ast.M, Ast.D) }
    | M { Ast.DestOne Ast.M }
    | D { Ast.DestOne Ast.D }
    | A { Ast.DestOne Ast.A }
    ;

compute:
    | ZERO; { Ast.Selection Ast.Zero }
    | ONE; { Ast.Selection Ast.One }
    | MINUS;ONE; { Ast.Minus (Ast.Zero, Ast.One) }
    | D; { Ast.Selection (Ast.Reg Ast.D)  }
    | A; { Ast.Selection (Ast.Reg Ast.A) }
    | M; { Ast.Selection (Ast.Reg Ast.M) }
    | EXCL; D; { Ast.Not (Ast.Reg Ast.D) } 
    | EXCL; A; { Ast.Not (Ast.Reg Ast.A)  }
    | EXCL; M; { Ast.Not (Ast.Reg Ast.M) }
    | MINUS; D; { Ast.Minus (Ast.Zero, Ast.Reg Ast.D) }
    | MINUS; A; { Ast.Minus (Ast.Zero, Ast.Reg Ast.A) }
    | MINUS; M; { Ast.Minus (Ast.Zero, Ast.Reg Ast.M) }
    | D; PLUS; ONE; { Ast.Plus(Ast.Reg Ast.D, Ast.One) }
    | A; PLUS; ONE; { Ast.Plus(Ast.Reg Ast.A, Ast.One) }
    | M; PLUS; ONE; { Ast.Plus(Ast.Reg Ast.M, Ast.One) }
    | D; MINUS; ONE; { Ast.Minus(Ast.Reg Ast.D, Ast.One) }
    | A; MINUS; ONE; { Ast.Minus(Ast.Reg Ast.A, Ast.One) }
    | M; MINUS; ONE; { Ast.Minus(Ast.Reg Ast.M, Ast.One) }
    | D; PLUS; A; { Ast.Plus(Ast.Reg Ast.D, Ast.Reg Ast.A) }
    | D; PLUS; M; { Ast.Plus(Ast.Reg Ast.D, Ast.Reg Ast.M) }
    | D; MINUS; A; { Ast.Minus(Ast.Reg Ast.D, Ast.Reg Ast.A) }
    | D; MINUS; M; { Ast.Minus(Ast.Reg Ast.D, Ast.Reg Ast.M) }
    | A; MINUS; D; { Ast.Minus(Ast.Reg Ast.A, Ast.Reg Ast.D) }
    | M; MINUS; D; { Ast.Minus(Ast.Reg Ast.M, Ast.Reg Ast.D ) }
    | D; AND; A; { Ast.And(Ast.Reg(Ast.D), Ast.Reg(Ast.A)) }
    | D; AND; M; { Ast.And(Ast.Reg Ast.D, Ast.Reg Ast.M) }
    | D; OR; A; { Ast.Or(Ast.Reg Ast.D, Ast.Reg Ast.A) }
    | D; OR; M; { Ast.Or(Ast.Reg Ast.D, Ast.Reg Ast.M ) }
    ;

jump:
    | NULL; { Ast.NoJump }
    | JGT; { Ast.Jgt }
    | JEQ; { Ast.Jeq }
    | JGE; { Ast.Jge }
    | JLT; { Ast.Jle }
    | JNE; { Ast.Jne }
    | JLE; { Ast.Jle }
    | JMP; { Ast.Jmp }
    ;

address:
    | n=NUMBER; { Ast.Address n }
    | ZERO; { Ast.Address 0 }
    | ONE; { Ast.Address 1 }
    | SP; { Ast.Address 0 }
    | LCL; { Ast.Address 1 }
    | ARG; { Ast.Address 2 }
    | THIS; { Ast.Address 3 }
    | THAT; { Ast.Address 4 }
    | KBD; { Ast.Address 24576 } 
    | SCREEN; { Ast.Address 16384 } 
    | r=REGISTER; { Ast.RegisterRef r }
    | a=ATOM; { Ast.LabelRef (fst a) }
    ;   
%%
