{
open Menhir_parser
open Lexing

exception LexError of string

let buf = Buffer.create 10000000

let[@inline] failwith msg = raise (LexError msg)

let[@inline] illegal c =
  failwith (Printf.sprintf "[lexer] unexpected character: '%c'" c)

let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
        pos_lnum = pos.pos_lnum + 1
      }

let get_line lexbuf =
    lexbuf.lex_curr_p.pos_lnum 

}

(* regular expressions *)
let whitespace = ' ' | '\t'
let newline = "\r\n" | '\r' | '\n'
let newline_eof = "\r\n" eof | '\r' eof | '\n' eof
let ident = ['A'-'Z' 'a'-'z' '0'-'9' '_' '.' '$']*
let number = ['0'-'9']*
let register = "R" ['0'-'9'] | "R1" ['0'-'5'] 

rule next_token = parse
  | eof { EOF }
  | whitespace+
    { next_token lexbuf }
  | newline_eof { next_token lexbuf }
  | newline
  { 
    if Buffer.length buf > 0 then (next_line lexbuf; Buffer.clear buf; NL) else next_token lexbuf
  } 
  | "//" ([^ '\n']* as c) { ignore c; next_token lexbuf; } 
  | register as r { Buffer.add_char buf 'r'; REGISTER (r) }
  | '0' { Buffer.add_char buf '0'; ZERO }
  | '1' { Buffer.add_char buf '1'; ONE }
  | '&' { Buffer.add_char buf '&'; AND }
  | '|' { Buffer.add_char buf '|'; OR }
  | '(' { Buffer.add_char buf '('; LPAR }
  | ')' { Buffer.add_char buf ')'; RPAR }
  | ';' { Buffer.add_char buf '/'; SEMICOLON }
  | '!' { Buffer.add_char buf '!'; EXCL }
  | '@' { Buffer.add_char buf '@'; AT }
  | "MD" { Buffer.add_char buf 'M'; MD }
  | "AMD" { Buffer.add_char buf 'A'; AMD }
  | "AM"  { Buffer.add_char buf 'A'; AM }
  | "AD"  { Buffer.add_char buf 'A'; AD }
  | 'D' { Buffer.add_char buf 'D'; D } 
  | 'M' { Buffer.add_char buf 'M'; M }
  | 'A' { Buffer.add_char buf 'A'; A }
  | "null" { Buffer.add_char buf 'n'; NULL }
  | "SP" { Buffer.add_string buf "SP"; SP }
  | "LCL" { Buffer.add_char buf 'l'; LCL }
  | "ARG" { Buffer.add_char buf 'a'; ARG }
  | "THIS" { Buffer.add_char buf 't'; THIS }
  | "THAT" { Buffer.add_char buf 't'; THAT }
  | "SCREEN" { Buffer.add_char buf 's'; SCREEN }
  | "KBD" { Buffer.add_string buf "KBD"; KBD }
  | '=' { Buffer.add_string buf "EQ"; EQ }
  | '-' { Buffer.add_string buf "MINUS"; MINUS }
  | '+' { Buffer.add_string buf "PLUS"; PLUS }
  | "JGT" { Buffer.add_string buf "JGT"; JGT }
  | "JEQ" { Buffer.add_string buf "JEQ"; JEQ }
  | "JGE" { Buffer.add_string buf "JGE"; JGE }
  | "JLT" { Buffer.add_string buf "JLT"; JLT }
  | "JNE" { Buffer.add_string buf "JNE"; JNE }
  | "JLE" { Buffer.add_string buf "JLE"; JLE }
  | "JMP" { Buffer.add_string buf "J"; JMP }
  (* lex identifiers last, so keywords are not lexed as identifiers *)
  | number as num { Buffer.add_string buf "NUMBER"; NUMBER ( int_of_string num) }
  | ident as atom { Buffer.add_string buf "ATOM"; ATOM (atom, get_line lexbuf) }
  (* no match? raise exception *)
  | _ as c { illegal c }
