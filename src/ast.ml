type binop = Add   | Sub   | Mul   | Div   | Mod
           | ElAdd | ElSub | ElMul | ElDiv | ElMod
           | Eq    | Neq   | Geq   | Leq | Lt | Gt
           | And   | Or    | Xor;;
           (* Bit ops not implemented *)
           (* | BitAnd | BitOr | BitXor | BitLs | BitRs;; *)
           
type unop = Not | Neg;;  (*Bit_not not not implemented *)

type scalar = LitInt of int | LitFloat of float;;

type datatype = Int | Float | Char | Bool | String | Tup of datatype | Fun
              | List of datatype | Matrix of datatype;; 

type stmt =
  Decl of vardecl
| Assign of varassign
| VecAssign of vecassign
| MatAssign of matassign
| Return of expr
| If of expr * stmt list * stmt list
| For of string * iterable * stmt list

and expr =
  NumLit of scalar
| CharLit of char
| StringLit of string
| BoolLit of bool
| TupLit of expr list
| ListLit of expr list
| FunLit of fun_type * stmt list
| MatrixLit of expr list list
(* Not implemented
| MatrixFunDef of expr * expr * func_item
*)
| MatrixInit of expr * expr
| BinOp of expr * expr * binop
| UnOp of expr * unop
| Id of string
| Attribute of string * string
| Call of string * expr list
(* Not implemented
| Pipe of expr * expr
*)
| MatAcc of string * mat_index * mat_index
| VecAcc of string * mat_index

and mat_index =
  MatIndex of expr
| MatSlice of expr * expr

and fun_type =
  FunType of arg list * return_type

and return_type = 
  ReturnData of datatype
(* Not implemented
| ReturnFun of fun_type
*) 

and arg =
  ArgId of string * datatype
(* Not implemented
| ArgFunId of string * fun_type
*)

and func_item =
  FunId of string
(* Not implemented
| FunItem of fun_type * stmt list
*)

and iterable =
  ItId of string
| ItListLit of expr list
| ItMatrixLit of expr list list
(* Not implemented
| ItMatrixFunDef of expr * expr * func_item
*)
| ItCall of string * expr list
| ItAttribute of string * string

and vardecl = {
    return_type : datatype;
    var_name    : string;
    body        : expr;
}

and varassign = {
    assign_name : string;
    new_val     : expr;
}

and vecassign = {
    vec_name    : string;
    index       : mat_index;
    vec_el_val  : expr;
} 

and matassign = {
    mat_name    : string;
    row_index   : mat_index;
    col_index   : mat_index;
    mat_el_val  : expr;
};; 

(* Start Symbol *)
type program = stmt list;; 
