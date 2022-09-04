(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Equal | Neq | Less | And | Or | Multiply | Divide | Modulus | Greater | GreaterOrEqual | LessOrEqual

type uniop = Not

type typ = Integer | Boolean | Float | String | Character | Arr of (typ * int) | None

type expr =
    IntegerLit of int
  | BooleanLit of bool
  | FloatLit of string
  | StringLit of string
  | CharacterLit of char
  | Id of string
  | NonePtr of typ
  | Binop of expr * op * expr
  | Uniop of uniop * expr
  | Assign of string * expr
  | Call of string * expr list
  | ArrayAccess of string * expr
  | ArrayLit of expr list
  | ArrAssign of string * expr * expr
  | AssignToVar of string * typ * expr


type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt
  | IfElse of expr * stmt * stmt
  | While of expr * stmt
  | For of expr * expr * expr * stmt
  | Return of expr

(* int x: name binding *)
type bind = typ * string

(* func_def: ret_typ fname formals locals body *)
type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
}

type program = bind list * func_def list

(* Pretty-printing functions *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Multiply -> "*"
  | Divide -> "/"
  | Modulus -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | And -> "&&"
  | Or -> "||"
  | LessOrEqual -> "<="
  | GreaterOrEqual -> ">="
  | Greater -> ">"

let string_of_uniop = function
  | Not -> "!"


let rec string_of_typ = function
    Integer -> "integer"
  | Boolean -> "boolean"
  | Float -> "float"
  | String -> "string"
  | Character -> "character"
  | None -> "none"
  | Arr(t, _) -> string_of_typ t ^ "[]"

let rec string_of_expr = function
    IntegerLit(l) -> string_of_int l
  | BooleanLit(true) -> "true"
  | BooleanLit(false) -> "false"
  | StringLit(s) -> s
  | FloatLit(f) -> f
  | NonePtr(t) -> string_of_typ t ^ " None"
  | CharacterLit(c) -> (String.make 1 c)
  | ArrayAccess(s,e) -> s ^ "[" ^ string_of_expr e ^ "]"
  | ArrayLit(e) -> "[" ^ String.concat "," (List.map string_of_expr (List.rev e)) ^ "]"
  | ArrAssign(s,e1,e2) -> s ^ "[" ^ string_of_expr e1 ^ "] = " ^ string_of_expr e2
  | Id(s) -> s
  | AssignToVar(s,t,e) -> string_of_typ t ^ " " ^ s ^ " = " ^ string_of_expr e
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Uniop(o, e) -> string_of_uniop o ^ " " ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"

let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s1) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1
  | IfElse(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) -> "for (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ", " ^ string_of_expr e3 ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  "function " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ") -> " ^ string_of_typ fdecl.rtyp ^ " {\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
