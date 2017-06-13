{ open Parser }

let NUM = ['0'-'9']
let CHAR = ['a'-'z' 'A'-'Z']

rule token = parse
(* Whitespace *)
[' ' '\t' '\r' '\n'] { token lexbuf }
(* Comments *)
| "/*" { multi_comment lexbuf }
| "//" { single_comment lexbuf }
(* String and character literals *)
| '\"'([^'\"' '\t' '\r' '\n']+ as lit)'\"' { STRING_LIT(lit) }
| '\''([^'\'' '\t' '\r' '\n'] as lit)'\'' { CHAR_LIT(lit) }
(* Arithmetic Operators *)
| '+'  { PLUS }
| '-'  { MINUS }
| '*'  { MULTIPLY }
| '/'  { DIVIDE }
| '%'  { MODULO }
(* Element-wise Arithmetic Operators *)
(* Not implemented
| ".+" { ELEM_PLUS }
| ".-" { ELEM_MINUS }
| ".*" { ELEM_MULTIPLY }
| "./" { ELEM_DIVIDE }
| ".%" { ELEM_MODULO }
*)
(* Comparison Operators *)
| "==" { EQ }
| "!=" { NEQ }
| ">="  { GEQ }
| "<="  { LEQ }
| ">"  { GT }
| "<"  { LT }
(* Boolean Operators *)
| "and" { AND }
| "or"  { OR }
| "xor" { XOR }
| "not" { NOT }
(* Bitwise Operators *)
(* Not supported
| "&&" { BIT_AND }
| "||" { BIT_OR }
| "*|" { BIT_XOR }
| "<<" { BIT_LS }
| ">>" { BIT_RS }
| "!"  { BIT_NOT }
**)
(* Assignment Operators *)
| '='    { ASSIGN }
| '-''>' { PRODUCES } (* used 2 chars instead of 1 string to work around syntax highlighter bug *)
(* Function Operators *)
| '|' { PIPE }
(* Delimeters *)
| '{' { LBRACE }
| '}' { RBRACE }
| '[' { LBRACKET }
| ']' { RBRACKET }
| '(' { LPAREN }
| ')' { RPAREN }
| ';' { SEMI }
| ':' { COLON }
| ',' { COMMA }
| '.' { PERIOD }
(* Control Flow *)
| "for"    { FOR }
| "in"     { IN }
| "if"     { IF }
| "else"   { ELSE }
| "return" { RETURN }
(* Types *)
| "bool"   { BOOL }
| "int"    { INT }
| "float"  { FLOAT }
| "char"   { CHAR }
| "fun"    { FUN }
| "String" { STRING }
| "Tup"    { TUP }
| "List"   { LIST }
| "Matrix" { MATRIX }
(* Scalar literals *)
| NUM*'.'NUM+ as lit { FLOAT_LIT(Ast.LitFloat(float_of_string lit)) }
| NUM+        as lit { INT_LIT(Ast.LitInt(int_of_string lit)) }
(* Boolean literals *)
| "True"  { BOOL_LIT(true) }
| "False" { BOOL_LIT(false) }
(* Identifier *)
| CHAR+(CHAR | NUM | '_' )* as lit { ID(lit) }
(* End of file and invalid characters *)
| eof { EOF }
| _ as lit { raise (Failure("illegal character " ^ Char.escaped lit)) }

and multi_comment = parse
  "*/" { token lexbuf }
| _    { multi_comment lexbuf }

and single_comment = parse
  '\n' { token lexbuf }
| _    { single_comment lexbuf }
