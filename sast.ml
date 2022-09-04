(* Semantically-checked Abstract Syntax Tree and functions for printing it *)
(* do the semantic check. given an AST, check the semantics and check the scope. then insert the type info to the AST to contrast? the SAST?.
throw errors when fine like mistmatched type, try to access vars that are undefined, etc*)

open Ast

type sexpr = typ * sx
and sx =
    SIntegerLit of int
  | SBooleanLit of bool
  | SFloatLit of string
  | SCharacterLit of char
  | SStringLit of string
  | SArrayAccess of string * sexpr
  | SArrayLit of sexpr list
  | SArrAssign of string * sexpr * sexpr
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUniop of uniop * sexpr
  | SAssign of string * sexpr
  | SAssignToVar of string * typ * sexpr
  | SCall of string * sexpr list
  | SNonePtr of typ

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt
  | SIfElse of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SReturn of sexpr

(* func_def: ret_typ fname formals locals body *)
type sfunc_def = {
  srtyp: typ;
  sfname: string;
  sformals: bind list;
  slocals: bind list;
  sbody: sstmt list;
}

type sprogram = bind list * sfunc_def list



(* Pretty-printing functions *)
let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
        SIntegerLit(l) -> string_of_int l
      | SBooleanLit(true) -> "true"
      | SBooleanLit(false) -> "false"
      | SStringLit(s) -> s
      | SFloatLit(f) -> f
      | SCharacterLit(c) -> (String.make 1 c)
      | SAssignToVar(s, t, e) -> string_of_typ t ^ " " ^ s ^ " == " ^ string_of_sexpr e
      | SUniop(o, e) -> string_of_uniop o ^ " " ^ string_of_sexpr e
      | SId(s) -> s
      | SNonePtr(t) -> string_of_typ t ^ " None"
      | SBinop(e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
      | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
      | SArrayAccess(s,e) -> s ^ "[" ^ string_of_sexpr e ^ "]"
      | SArrayLit(e) -> "[" ^ String.concat "," (List.map string_of_sexpr (List.rev e)) ^ "]"
      | SArrAssign(s, e1, e2) -> s ^ "[" ^ string_of_sexpr e1 ^ "] = " ^ string_of_sexpr e2
      | SCall(f, el) ->
          f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
    ) ^ ")"

let rec string_of_sstmt = function
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n"
  | SIf(e, s1) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
                       string_of_sstmt s1
  | SIfElse(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
                       string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SFor(e1, e2, e3, s) -> "for (" ^ string_of_sexpr e1 ^ ", " ^ string_of_sexpr e2 ^ ", " ^ string_of_sexpr e3 ^ " " ^ string_of_sstmt s

let string_of_sfdecl fdecl =
  string_of_typ fdecl.srtyp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  "\n\nSementically checked program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
