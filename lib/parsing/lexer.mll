{
  open Lexing
  open Parser

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = lexbuf.lex_curr_pos }

  let filename = lexbuf.lex_curr_p.pos_fname (* gets the name of the file that was passed in from main.ml *)
  let line = lexbuf.lex_curr_p.pos_lnum (* gets the name of the line from current position *)

  let metainfo = (filename, line) (* puts the filename and line number into a tuple *)
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let integer = '-'? digit+
let identifier = (alpha | '_' ) (alpha | digit | '_')*

rule read = parse
  | white              { read lexbuf }
  | "--"               { read_single_line_comment lexbuf }
  | "{-"               { read_multi_line_comment lexbuf }
  | '('                { LPAREN (metainfo) }
  | ')'                { RPAREN (metainfo) }
  | '['                { LBRACKET (metainfo) }
  | ']'                { RBRACKET (metainfo) }
  | ','                { COMMA (metainfo) }
  | '.'                { DOT (metainfo) }
  | ':'                { COLON (metainfo) }
  | ';'                { SEMICOLON (metainfo) }
  | '+'                { PLUS (metainfo) }
  | '-'                { MINUS (metainfo) }
  | '*'                { TIMES (metainfo) }
  | '/'                { DIV (metainfo) }
  | "&&"               { AND (metainfo) }
  | "||"               { OR (metainfo) }
  | "="                { EQ (metainfo) }
  | "!="               { NEQ (metainfo) }
  | "<"                { LT (metainfo) }
  | "<="               { LEQ (metainfo) }
  | ">"                { GT (metainfo) }
  | ">="               { GEQ (metainfo) }
  | '|'                { VERTICAL (metainfo) }
  | '_'                { UNDERSCORE (metainfo) }
  | ":="               { COLONEQ } (* add rest after meeting *)
  | "->"               { ARROW }
  | "~>"               { TILDE_ARROW }
  | "unit"             { UNIT_T }
  | "int"              { INT_T }
  | "string"           { STRING_T }
  | "bool"             { BOOL_T }
  | "fun"              { FUN }
  | "type"             { TYPE }
  | "true"             { TRUE }
  | "false"            { FALSE }
  | "if"               { IF }
  | "then"             { THEN }
  | "else"             { ELSE }
  | "match"            { MATCH }
  | "with"             { WITH }
  | "let"              { LET }
  | "in"               { IN }
  | "fst"              { FST }
  | "snd"              { SND }
  | "left"             { LEFT }
  | "right"            { RIGHT } (* up to here? *)
  | integer as s       { INT (int_of_string s * metainfo) }
  | identifier as s    { ID (s * metainfo) }
  | '"'                { read_string (Buffer.create 17) lexbuf }
  | newline            { next_line lexbuf; read lexbuf }
  | _                  { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof                { EOF }

and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf * metainfo) }
  | '\\' ('/' | '\\' | 'b' | 'f' | 'n' | 'r' | 't' as esc)
    { let c = match esc with
        | '/'  -> '/'
        | '\\' -> '\\'
        | 'b'  -> '\b'
        | 'f'  -> '\012'
        | 'n'  -> '\n'
        | 'r'  -> '\r'
        | 't'  -> '\t'
        | _    -> assert false
      in Buffer.add_char buf c;
      read_string buf lexbuf
    }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _   { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError "String is not terminated") }

and read_single_line_comment = parse
  | newline { next_line lexbuf; read lexbuf }
  | _       { read_single_line_comment lexbuf }
  | eof     { EOF }

and read_multi_line_comment = parse
  | "-}"    { read lexbuf }
  | newline { next_line lexbuf; read_multi_line_comment lexbuf }
  | _       { read_multi_line_comment lexbuf }
  | eof     { raise (SyntaxError "Comment is not terminated") }
