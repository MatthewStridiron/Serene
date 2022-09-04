(* Semantic checking for the Serene compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

module HashtblString =
        struct
        type t = string
        let equal = ( = )
        let hash = Hashtbl.hash
        end;;

module StringHash = Hashtbl.Make(HashtblString);;


(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =

  (* Verify a list of bindings has no duplicate names *)
  (* two different vars or function can't have the same name *)
  let check_binds (kind : string) (binds : (typ * string) list) =
    let rec dups = function
        [] -> ()
      | ((_,n1) :: (_,n2) :: _) when n1 = n2 ->
        raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (* Make sure no globals duplicate *)
  check_binds "global" globals;

  let built_in_decls =
    let add_builtin string_map (name, ty) = StringMap.add name {
      rtyp = Integer;
      fname = name;
      formals = [(ty, "x")];
      locals = [];
      body = [] } string_map in
    List.fold_left add_builtin StringMap.empty [
      ("print", Integer);
      ("printfloat", Float);
      ("printstring", String);
    ] in
  let built_in_decls =
    StringMap.add "maximum" {
      rtyp = Integer;
      fname = "maximum";
      formals = [(Arr(Integer, 1), "arr"); (Integer, "n")];
      locals = [];
      body = []
    } built_in_decls in
  let built_in_decls =
    StringMap.add "minimum" {
      rtyp = Integer;
      fname = "minimum";
      formals = [(Arr(Integer, 1), "arr"); (Integer, "n")];
      locals = [];
      body = [] 
    } built_in_decls in
  let built_in_decls =
    StringMap.add "sum" {
      rtyp = Integer;
      fname = "sum";
      formals = [(Arr(Integer, 1), "arr"); (Integer, "n")];
      locals = [];
      body = []
    } built_in_decls in
  let built_in_decls =
    StringMap.add "product" {
      rtyp = Integer;
      fname = "product";
      formals = [(Arr(Integer, 1), "arr"); (Integer, "n")];
      locals = [];
      body = []
    } built_in_decls in
  let built_in_decls =
    StringMap.add "reverse" {
      rtyp = Arr(Integer, 1);
      fname = "reverse";
      formals = [(Arr(Integer, 1), "arr"); (Integer, "n")];
      locals = [];
      body = []
    } built_in_decls in
  let built_in_decls =
    StringMap.add "substring" {
      rtyp = String;
      fname = "substring";
      formals = [(String, "str");  (Integer, "start"); (Integer, "end")];
      locals = [];
      body = []
      } built_in_decls in
  let built_in_decls = 
    StringMap.add "findchar" {
      rtyp = Integer;
      fname = "findchar";
      formals = [(String, "str"); (Character, "char")];
      locals = [];
      body = []
      } built_in_decls in
  let built_in_decls = 
    StringMap.add "replaceletters" {
      rtyp = String;
      fname = "replaceletters";
      formals = [(String, "str"); (Character, "char1"); (Character, "char2")];
      locals = [];
      body = []
      } built_in_decls in
  let built_in_decls = 
    StringMap.add "scrambleletters" {
      rtyp = String;
      fname = "scrambleletters";
      formals = [(String, "str")];
      locals = [];
      body = []
      } built_in_decls in
  let built_in_decls =
    StringMap.add "power" {
      rtyp = Integer;
      fname = "power";
      formals = [(Integer, "base"); (Integer, "power")];
      locals = [];
      body = []
    } built_in_decls in
  let built_in_decls =
    StringMap.add "absolute" {
      rtyp = Integer;
      fname = "absolute";
      formals = [(Integer, "n");];
      locals = [];
      body = []
    } built_in_decls in
  let built_in_decls =
    StringMap.add "printboolean" {
      rtyp = Integer;
      fname = "printboolean";
      formals = [(Boolean, "b");];
      locals = [];
      body = []
    } built_in_decls in
  let built_in_decls =
    StringMap.add "fibonacci" {
      rtyp = Integer;
      fname = "fibonacci";
      formals = [(Integer, "n");];
      locals = [];
      body = []
    } built_in_decls in
  let built_in_decls =
    StringMap.add "factorial" {
      rtyp = Integer;
      fname = "factorial";
      formals = [(Integer, "n");];
      locals = [];
      body = []
    } built_in_decls in
  let built_in_decls = (* print array of ints*)
    StringMap.add "printarray" {
      rtyp = Integer;
      fname = "printarray";
      formals = [(Arr(Integer, 1), "arr"); (Integer, "n")];
      locals = [];
      body = []
    } built_in_decls in
  let built_in_decls =
    StringMap.add "touppercase" {
      rtyp = String;
      fname = "touppercase";
      formals = [(String, "str")];
      locals = [];
      body = []
    } built_in_decls in
  let built_in_decls =
    StringMap.add "tolowercase" {
      rtyp = String;
      fname = "tolowercase";
      formals = [(String, "str")];
      locals = [];
      body = []
    } built_in_decls in
  let built_in_decls =
    StringMap.add "reversestring" {
      rtyp = String;
      fname = "reversestring";
      formals = [(String, "str")];
      locals = [];
      body = []
    } built_in_decls in
  let built_in_decls =
    StringMap.add "randomnumber" {
      rtyp = Integer;
      fname = "randomnumber";
      formals = [(Integer, "min"); (Integer, "max")];
      locals = [];
      body = []
    } built_in_decls in
  let built_in_decls =
    StringMap.add "randomletter" {
      rtyp = String;
      fname = "randomletter";
      formals = [];
      locals = [];
      body = []
    } built_in_decls in
  let built_in_decls =
    StringMap.add "repeatstring" {
      rtyp = String;
      fname = "repeatstring";
      formals = [(String, "s"); (Integer, "n")];
      locals = [];
      body = []
    } built_in_decls in
  let built_in_decls =
    StringMap.add "greatestcommondenominator" {
      rtyp = Integer;
      fname = "greatestcommondenominator";
      formals = [(Integer, "x"); (Integer, "y")];
      locals = [];
      body = []
    } built_in_decls in
  let built_in_decls =
    StringMap.add "range" {
      rtyp = Arr(Integer, 1);
      fname = "range";
      formals = [(Integer, "min"); (Integer, "max")];
      locals = [];
      body = []
    } built_in_decls in
  let built_in_decls =
    StringMap.add "rangewithstep" {
      rtyp = Arr(Integer, 1);
      fname = "rangewithstep";
      formals = [(Integer, "min"); (Integer, "max"); (Integer, "step")];
      locals = [];
      body = []
    } built_in_decls in
  let built_in_decls =
    StringMap.add "isprime" {
      rtyp = Boolean;
      fname = "isprime";
      formals = [(Integer, "n")];
      locals = [];
      body = []
    } built_in_decls in
  let built_in_decls =
    StringMap.add "distance" {
      rtyp = Integer;
      fname = "distance";
      formals = [(Integer, "x1"); (Integer, "y1"); (Integer, "x2"); (Integer, "y2")];
      locals = [];
      body = []
    } built_in_decls in
  let built_in_decls =
    StringMap.add "concatenate" {
      rtyp = String;
      fname = "concatenate";
      formals = [(String, "s1"); (String, "s2")];
      locals = [];
      body = []
    } built_in_decls in

  (* Add function name to symbol table *)
  (* fd = function definition, f = function name *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
      _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ ->  StringMap.add n fd map
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in

  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  (*added 5-4-22*)
 let rec is_array (array_typ, index) =
        (match array_typ with
        Arr(typ, _) -> is_array(typ, index+1)
        | _ -> (array_typ, index))  

  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_func func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;
    check_binds "local" func.locals;

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)

(*updated 5-4-22*)
(*Citation : http://www.cs.columbia.edu/~sedwards/classes/2021/4115-spring/reports/JavaLite.pdf *)

    let check_assign lvalue_typ rvalue_typ err =
      if lvalue_typ = rvalue_typ then lvalue_typ else
        (match lvalue_typ with
         Arr _ -> (match rvalue_typ with
                Arr _ -> let r_arr = is_array (rvalue_typ,0) in
                        let l_arr = is_array (lvalue_typ, 0) in
                        if r_arr = l_arr then rvalue_typ else raise(Failure err)
                | _ -> raise(Failure err))
        | _ -> raise(Failure err))
        
    in

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
        StringMap.empty (globals @ func.formals @ func.locals )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    let is_arr_ty(v, ty) = match ty with
        Arr(t, _) -> t
        | _ -> raise(Failure("cannot access element " ^ v ^ " of type " ^ string_of_typ ty))

    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec check_expr e =

        let is_null expr l error =
        let (r, e') = check_expr expr in
        let typ = check_assign l r error in
        if e' = (SNonePtr None) then (typ, (SNonePtr typ)) else (typ, e')

        in

  (match e with
        IntegerLit l -> (Integer, SIntegerLit l)
      | BooleanLit l -> (Boolean, SBooleanLit l)
      | FloatLit l -> (Float, SFloatLit l)
      | StringLit l -> (String, SStringLit l)
      | CharacterLit l -> (Character, SCharacterLit l)
      | NonePtr t -> (t, SNonePtr t)
      | Id var -> (type_of_identifier var, SId var)
      | AssignToVar(var, ty, e) as ex ->
        let (rt, e') = check_expr e in
        let err = "illegal assignment " ^ string_of_typ ty ^ " = " ^
                  string_of_typ rt ^ " in " ^ string_of_expr ex
        in
        (check_assign ty rt err, SAssignToVar(var, ty, (rt, e')))
      | Assign(var, e) as ex ->
        let lt = type_of_identifier var
        and (rt, _) = check_expr e in
        let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                  string_of_typ rt ^ " in " ^ string_of_expr ex
        in
        let (ty, e') = is_null e lt err
        in let _ = (match ty with
        _ -> ignore 1)
        in (ty, SAssign(var, (ty, e')))
      | Uniop(op, e1) ->
          let (t1, e1') = check_expr e1
        in
        (Boolean, SUniop(op, (t1, e1')))
      | Binop(e1, op, e2) as e ->
        let (t1, e1') = check_expr e1
        and (t2, e2') = check_expr e2 in
        let err = "illegal binary operator " ^
                  string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                  string_of_typ t2 ^ " in " ^ string_of_expr e
        in
        (* All binary operators require operands of the same type*)
        if t1 = t2 then
          (* Determine expression type based on operator and operand types *)
          let t = match op with
            | Add | Sub | Multiply | Divide | Modulus when t1 = Integer -> Integer
            | Add | Sub | Multiply | Divide | Modulus when t1 = Float -> Float
            | Equal | Neq -> Boolean
            | Less | Greater | LessOrEqual | GreaterOrEqual when (t1 = Integer) || (t1 = Float) -> Boolean
            | And | Or when t1 = Boolean -> Boolean
            | _ -> raise (Failure err)
          in
          (t, SBinop((t1, e1'), op, (t2, e2')))
        else raise (Failure err)
      | Call(fname, args) as call ->
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ string_of_expr call))
      (*updated 5-4-22*)
          else let check_call (ft, _) e =
               let (et, e') = check_expr e in
               let err = "illegal argument found " ^ string_of_typ et ^
                         " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
               in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.rtyp, SCall(fname, args'))
(* Citation: http://www.cs.columbia.edu/~sedwards/classes/2021/4115-spring/reports/JavaLite.pdf
Their arrays happened to look similar to ours *)

      | ArrayLit(expr1) as arraylit ->
          let ty_inconsistentcy = "array type inconsistency " ^ string_of_expr arraylit in
          let first_expr = List.hd expr1 in
          let (first_typ, _) = check_expr first_expr in
          let (arr_length, arr_ty_expr) = List.fold_left (fun(a,b) e ->
                let (et, e') = check_expr e in
                let is_arr = (
                  match et with
                  Arr _ -> true
                  | _ -> false
                ) in
                if ((et=first_typ) || is_arr) then (a+1, (et,e')::b) else (a, (et,e')::b)) (0, []) expr1 in
          if arr_length != List.length expr1 then
            raise(Failure ty_inconsistentcy)
          else
            let arr_type = Arr(first_typ, arr_length) in
          (arr_type, SArrayLit(arr_ty_expr))
      | ArrayAccess(var,expr) as arrayaccess ->
          let (typ, e') = check_expr expr in
          if typ != Integer then
            raise(Failure(string_of_expr expr ^ "is not of the correct type in " ^ string_of_expr arrayaccess))
          else
            let var_ty = type_of_identifier var in
            let expr_ty = is_arr_ty (var, var_ty)
            in (expr_ty, SArrayAccess(var, (typ,e')))
      | ArrAssign(var, expr1, expr2) as arrassign ->
          let (t, expr1') = check_expr expr1 in
          if t != Integer then
            raise(Failure(string_of_expr expr1 ^ "is not of the correct type in " ^ string_of_expr arrassign))
          else
            let var_ty = type_of_identifier var in
            let expr_ty = is_arr_ty (var, var_ty) in
            let (rt, _) = check_expr expr2 in
            let err = "illegal assignment " ^ string_of_typ expr_ty ^ " = " ^
            string_of_typ rt ^ "in " ^ string_of_expr arrassign in
            let (typ, expr2') = is_null expr2 expr_ty err in
            (typ, SArrAssign(var, (t, expr1'), (typ, expr2')))
)          
         
    in

    let check_bool_expr e =
      let (t, e') = check_expr e in
      match t with
      | Boolean -> (t, e')
      |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))

    in

    let rec check_stmt_list =function
        [] -> []
      | Block sl :: sl'  -> check_stmt_list (sl @ sl') (* Flatten blocks *)
      | s :: sl -> check_stmt s :: check_stmt_list sl
    (* Return a semantically-checked statement i.e. containing sexprs *)
    and check_stmt = function
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
        Block sl -> SBlock (check_stmt_list sl)
      | Expr e -> SExpr (check_expr e)
      | If(e, st1) ->
        SIf(check_bool_expr e, check_stmt st1)
      | IfElse(e, st1, st2) ->
        SIfElse(check_bool_expr e, check_stmt st1, check_stmt st2)
      | For(e1, e2, e3, st) ->
        SFor(check_expr e1, check_expr e2, check_expr e3, check_stmt st)
      | While(e, st) ->
        SWhile(check_bool_expr e, check_stmt st)
      | Return e ->
        let (t, e') = check_expr e in
        if t = func.rtyp then SReturn (t, e')
        else
        raise (Failure("return gives " ^ string_of_typ t ^ " expected " ^
        string_of_typ func.rtyp ^ " in " ^ string_of_expr e))

in (* body of check_func *)
    { srtyp = func.rtyp;
      sfname = func.fname;
      sformals = func.formals;
      slocals  = func.locals;
      sbody = check_stmt_list func.body
    }
  in
  (globals, List.map check_func functions)
