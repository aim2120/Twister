%{ open Ast %}

/* Ocamlyacc parser for Twister */

/* Arithmetic Operators */
%token PLUS MINUS MULTIPLY DIVIDE MODULO

/* Element-wise Arithmetic Operators */
/* Not implemented
%token ELEM_PLUS ELEM_MINUS ELEM_MULTIPLY ELEM_DIVIDE ELEM_MODULO
*/

/* Comparison Operators */
%token EQ NEQ GT LT GEQ LEQ

/* Boolean Operators */
%token AND OR NOT XOR

/* Bitwise Operators */
/* Not supported
%token BIT_AND BIT_OR BIT_XOR BIT_LS BIT_RS BIT_NOT
*/

/* Assignment Operators */
%token ASSIGN PRODUCES

/* Function Operator */
%token PIPE

/* Delimeters */
%token LBRACE RBRACE LBRACKET RBRACKET LPAREN RPAREN SEMI COLON COMMA PERIOD

/* Control Flow */
%token FOR IN IF ELSE RETURN

/* Types */
%token BOOL INT FLOAT CHAR FUN STRING TUP LIST MATRIX

/* Booleans */
%token TRUE FALSE

/* Literals */
%token <Ast.scalar>   INT_LIT
%token <Ast.scalar>   FLOAT_LIT
%token <bool>         BOOL_LIT
%token <char>         CHAR_LIT
%token <string>       STRING_LIT

/* Identifier */
%token <string> ID

%token EOF

/* Operator precedence*/
%right COMMA
%right ASSIGN PRODUCES
%left LBRACE RBRACE
%left LBRACKET RBRACKET
%left PIPE
%left OR
%left XOR
%left AND
/* Not supported
%left BIT_OR
%left BIT_XOR
%left BIT_AND
*/
%left EQ NEQ
%left GT LT
%left GEQ LEQ
/* Not supported
%left BIT_LS BIT_RS
*/
/* Not implemented
%left ELEM_PLUS ELEM_MINUS
*/
%left PLUS MINUS
/* Not implemented
%left ELEM_MULTIPLY ELEM_DIVIDE ELEM_MODULO
*/
%left MULTIPLY DIVIDE MODULO
%right NOT NEG """ Not implemented BIT_NOT """
%right PERIOD

%start program
%type <Ast.program> program

%%

/* TODO make sure all lists are in the correct order (use List.rev) AIM 2/28/17 */

program:
stmts EOF { $1 }


/* list of statements */
stmts:
  stmt stmts    { $1 :: $2 }
| /* nothing */ { [] }
/* TODO question: do we want there to be empty statements? AIM 2/28/17 */

/* top-level statement */
stmt:
  vardecl                                                            { Decl $1 }
| varassign                                                          { Assign $1 }
| vecassign                                                          { VecAssign $1 }
| matassign                                                          { MatAssign $1 }
| RETURN expr SEMI                                                   { Return $2 }
| IF LPAREN expr RPAREN LBRACE stmts RBRACE                           { If($3, $6, []) }
| IF LPAREN expr RPAREN LBRACE stmts RBRACE ELSE LBRACE stmts RBRACE   { If($3, $6, $10) }
| FOR LPAREN ID IN iterable RPAREN LBRACE stmts RBRACE                { For($3, $5, $8) }

/* top-level variable declaration */
vardecl:
  datatype ID ASSIGN expr SEMI {{
    return_type = $1;
    var_name = $2;
    body = $4;
  }}

/* top-level variable assignment */
varassign:
  ID ASSIGN expr SEMI {{
    assign_name = $1;
    new_val = $3;
  }}

/* top-level variable assignment */
vecassign:
  ID LBRACKET mat_index RBRACKET ASSIGN expr SEMI {{
    vec_name = $1;
    index = $3;
    vec_el_val = $6;
  }}

  /* top-level variable assignment */
matassign:
  ID LBRACKET mat_index RBRACKET LBRACKET mat_index RBRACKET ASSIGN expr SEMI {{
    mat_name = $1;
    row_index = $3;
    col_index = $6;
    mat_el_val = $9;
  }}

/* expression */
expr:
/* literals */
  INT_LIT                                                                  { NumLit $1 }
| FLOAT_LIT                                                                { NumLit $1 }
| CHAR_LIT                                                                 { CharLit $1 }
| STRING_LIT                                                               { StringLit $1 }
| TRUE                                                                     { BoolLit true }
| FALSE                                                                    { BoolLit false }
| LPAREN tup_list RPAREN                                                   { TupLit $2 }
| LBRACE obj_list RBRACE                                                   { ListLit $2 }
| fun_type LBRACE stmts RBRACE                                             { FunLit($1, $3) }
| LBRACKET mat_rows RBRACKET                                               { MatrixLit $2 }
/* Not implemented
| MATRIX LPAREN LPAREN expr COMMA expr RPAREN COMMA func_item RPAREN       { MatrixFunDef($4, $6, $9) }
*/
| MATRIX LPAREN expr COMMA expr RPAREN                                     { MatrixInit($3, $5) }
/* operators */
| expr PLUS expr                                                           { BinOp($1, $3, Add) }
| expr MINUS expr                                                          { BinOp($1, $3, Sub) }
| expr MULTIPLY expr                                                       { BinOp($1, $3, Mul) }
| expr DIVIDE expr                                                         { BinOp($1, $3, Div) }
| expr MODULO expr                                                         { BinOp($1, $3, Mod) }
/* Not implemented
| expr ELEM_PLUS expr                                                      { BinOp($1, $3, ElAdd) }
| expr ELEM_MINUS expr                                                     { BinOp($1, $3, ElSub) }
| expr ELEM_MULTIPLY expr                                                  { BinOp($1, $3, ElMul) }
| expr ELEM_DIVIDE expr                                                    { BinOp($1, $3, ElDiv) }
| expr ELEM_MODULO expr                                                    { BinOp($1, $3, ElMod) }
*/
| expr EQ expr                                                             { BinOp($1, $3, Eq) }
| expr NEQ expr                                                            { BinOp($1, $3, Neq) }
| expr GT expr                                                             { BinOp($1, $3, Gt) }
| expr LT expr                                                             { BinOp($1, $3, Lt) }
| expr GEQ expr                                                            { BinOp($1, $3, Geq) }
| expr LEQ expr                                                            { BinOp($1, $3, Leq) }
| expr AND expr                                                            { BinOp($1, $3, And) }
| expr OR expr                                                             { BinOp($1, $3, Or) }
| expr XOR expr                                                            { BinOp($1, $3, Xor) }
| NOT expr                                                                 { UnOp($2, Not) }
| MINUS expr %prec NEG                                                     { UnOp($2, Neg) }
/* Not supported
| expr BIT_AND expr                                                        { BinOp($1, $3, BitAnd) }
| expr BIT_OR expr                                                         { BinOp($1, $3, BitOr) }
| expr BIT_XOR expr                                                        { BinOp($1, $3, BitXor) }
| expr BIT_LS expr                                                         { BinOp($1, $3, BitLs) }
| expr BIT_RS expr                                                         { BinOp($1, $3, BitRs) }
| BIT_NOT expr                                                             { UnOp($2, BitNot) }
*/
/* ID and ID access */
| ID                                                                       { Id $1 }
| ID PERIOD ID                                                             { Attribute($1, $3) }
| ID LPAREN actuals_list_opt RPAREN                                        { Call($1, $3) }
/* Matrix access */
| ID LBRACKET mat_index RBRACKET LBRACKET mat_index RBRACKET               { MatAcc($1, $3, $6) }
| ID LBRACKET mat_index RBRACKET                                           { VecAcc($1, $3) }
/* 
""" pipe """
| expr PIPE expr                                                           { Pipe($1, $3) }
*/
/* parentheses */
| LPAREN expr RPAREN                                                       { $2 }

/* Index into a Matrix */
mat_index:
  expr COLON expr   { MatSlice($1, $3) }
| expr              { MatIndex $1 }

/* possibly empty list of actual arguments */
actuals_list_opt:
  actuals_list  { $1 }
| /* nothing */ { [] }

/* a list of actual arguments */
actuals_list:
  expr COMMA actuals_list { $1 :: $3 }
| expr                 { [$1] }

/* possibly empty list of expressions (for List) */
obj_list:
  expr COMMA obj_list { $1 :: $3 }
| expr                { [$1] }
| /* nothing */       { [] }

/* list of at least 2 expressions (for Tup) */
tup_list:
  expr COMMA tup_list { $1 :: $3 }
| expr COMMA expr     { $1 :: [$3] }

/* a possibly empty list of formal arguments */
arg_list:
  arg COMMA arg_list { $1 :: $3 }
| arg                { [$1] }
| /* nothing */      { [] }

/* either an ID with a datatype or an assignment with a datatype */
arg:
  ID COLON datatype                             { ArgId($1, $3) }
/* Not implemented
| ID COLON LPAREN fun_type RPAREN               { ArgFunId($1, $4) }
*/

/* list of Matrix rows */
mat_rows:
| mat_row SEMI mat_rows { $1 :: $3 }
| mat_row               { [$1] }

/* list of Matrix items */
mat_row:
| expr COMMA mat_row { $1 :: $3 }
| expr               { [$1] }

/* either an ID or a function literal */
func_item:
  ID                               { FunId $1 }
/*
| fun_type LBRACE stmts RBRACE     { FunItem($1, $3) }
*/

/* Function type signature a map from a tuple of arguments to a return type */
fun_type:
  LPAREN arg_list RPAREN PRODUCES fun_return_type  {FunType($2, $5)}

/* Function return type: either a data type or
 a function type signature (functions are first-class, and can be returned from other functions!) */
fun_return_type:
/* Not implemented
  LPAREN fun_type RPAREN { ReturnFun($2)}
*/
| datatype { ReturnData($1)}

/* 
   either an ID or an iterable literal (List, Matrix)
   or a function that returns an iterable literal
*/
iterable:
  ID                                                         { ItId $1 }
| LBRACE obj_list RBRACE                                     { ItListLit $2 }
| LBRACKET mat_rows RBRACKET                                 { ItMatrixLit $2 }
/* Not implemented
| LPAREN LPAREN expr COMMA expr RPAREN COMMA func_item       { ItMatrixFunDef($3, $5, $8) }
*/
| ID LPAREN actuals_list_opt RPAREN                          { ItCall($1, $3) }
| ID PERIOD ID                                               { ItAttribute($1, $3) }

/* all possible datatypes (includes typed types) */
datatype:
  INT                     { Int }
| BOOL                    { Bool }
| FLOAT                   { Float }
| CHAR                    { Char }
| STRING                  { String }
| TUP LT datatype GT      { Tup $3}
| FUN                     { Fun }
| MATRIX LT datatype GT { Matrix $3 }
| LIST LT datatype GT   { List $3 }

