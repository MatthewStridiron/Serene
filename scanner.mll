(* Ocamllex scanner for Serene *)

{
  open Parser

  let remove_double_quotes s =
    match s with
    | _ when (String.length s <= 2) -> ""
    | _ -> String.sub s 1 ((String.length s) - 2)

  let remove_single_quotes s =
    String.get s 1
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
| [' ' '\t' '\r' '\n'] { token lexbuf }
| (['\n']+('~'[^'\n']*))* { token lexbuf }
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| '['      { LBRACK }
| ']'      { RBRACK }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { MULTIPLY } (* New *)
| '/'      { DIVIDE } (* New *)
| '%'      { MODULUS } (* New *)
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| '>'      { GT } (* New *)
| "<="     { LEQ } (* New *)
| ">="     { GEQ } (* New *)
| "and"    { AND } (* New *)
| "or"     { OR } (* New *)
| "not"    { NOT } (* New *)
| "&&"     { AND }
| "||"     { OR }
| "if"     { IF }
| "else"   { ELSE }
| "while"  { WHILE }
| "for"    { FOR } (* New *)
| "in"     { IN } (* New *)
| "->"     { ARROW } (* New *)
| "function" { FUNCTION } (* New *)
| "return" { RETURN }
| "integer"    { INTEGER }
| "boolean"   { BOOLEAN }
| "float"     { FLOAT } (* New *)
| "string" { STRING } (* New *)
| "character" { CHARACTER } (* New *)
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| "null"   { NONE }
| digit+ as lem  { ILIT(int_of_string lem) }
| (digit+)"."(digit+) as f { FLIT(f) } (* New *)
| '"' { STLIT (buffer (Buffer.create 300) lexbuf) }
| ('\''[' '-'~']?'\'') as c { CHLIT(remove_single_quotes c) }
| letter (digit | letter | '_')* as lem { ID(lem) }
| eof { EOF }
| "!~" { comment lexbuf }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
| "~!" { token lexbuf }
| _    { comment lexbuf }

and buffer buf = parse
  | '"'    { Buffer.contents buf }
  | _ as c { Buffer.add_char buf c; buffer buf lexbuf }

