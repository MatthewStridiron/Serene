(* IR generation: translate takes a semantically checked AST and
   produces LLVM IR

   LLVM tutorial: Make sure to read the OCaml version of the tutorial

   http://llvm.org/docs/tutorial/index.html

   Detailed documentation on the OCaml LLVM library:

   http://llvm.moe/
   http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "Serene" in (*module definition*)
(*all of this code returns the module, which contains the ir code*)

  (* Get types from the context *)
  let i32_t = L.i32_type context
  and i8_t = L.i8_type context
  and i1_t = L.i1_type context
  and float_t = L.float_type context
  and void_t = L.void_type context
  and str_t = L.pointer_type (L.i8_type context)
  in
  (* Return the LLVM type for a Serene type *)
  let rec ltype_of_typ = function
      A.Integer   -> i32_t (* if the var is an int, use 32 bits*)
    | A.Boolean  -> i1_t (* if the var is a bool, use 1 bit *)
    | A.Float -> float_t
    | A.String -> str_t
    | A.Character -> i8_t
    | A.Arr(typ, _) -> L.pointer_type (ltype_of_typ typ)
    | A.None -> void_t
  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init = match t with
        A.Float -> L.const_float (ltype_of_typ t) 0.0
        | A.String -> L.const_pointer_null (ltype_of_typ t)
        | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m (* <-- global variable definition *)
    in
    List.fold_left global_var StringMap.empty globals in

  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in
  let maximum_t : L.lltype =
    L.var_arg_function_type i32_t [||] in
  let maximum_func : L.llvalue =
  L.declare_function "maximum" maximum_t the_module in
  let minimum_t : L.lltype =
    L.var_arg_function_type i32_t [||] in
  let minimum_func : L.llvalue =
    L.declare_function "minimum" minimum_t the_module in
  let sum_t : L.lltype =
    L.var_arg_function_type i32_t [||] in
  let sum_func : L.llvalue =
    L.declare_function "sum" sum_t the_module in
  let product_t : L.lltype =
    L.var_arg_function_type i32_t [||] in
  let product_func : L.llvalue =
    L.declare_function "product" product_t the_module in
  let reverse_t : L.lltype =
    L.var_arg_function_type (L.pointer_type i32_t) [||] in
  let reverse_func : L.llvalue =
    L.declare_function "reverse" reverse_t the_module in
  let substring_t : L.lltype =
    L.var_arg_function_type str_t [||] in
  let substring_func : L.llvalue =
    L.declare_function "substring" substring_t the_module in
  let replaceletters_t : L.lltype =
    L.var_arg_function_type str_t [||] in
  let replaceletters_func : L.llvalue =
    L.declare_function "replaceletters" replaceletters_t the_module in
  let scrambleletters_t : L.lltype =
    L.var_arg_function_type str_t [||] in
  let scrambleletters_func : L.llvalue =
    L.declare_function "scrambleletters" scrambleletters_t the_module in
  let findchar_t : L.lltype =
    L.var_arg_function_type i32_t [||] in
  let findchar_func : L.llvalue =
    L.declare_function "findchar" findchar_t the_module in
  let power_t : L.lltype =
    L.var_arg_function_type i32_t [||] in
  let power_func : L.llvalue =
    L.declare_function "power" power_t the_module in
  let absolute_t : L.lltype =
    L.var_arg_function_type i32_t [||] in
  let absolute_func : L.llvalue =
    L.declare_function "absolute" absolute_t the_module in
  let printboolean_t : L.lltype =
    L.var_arg_function_type i32_t [||] in
  let printboolean_func : L.llvalue =
    L.declare_function "printboolean" printboolean_t the_module in
  let printarray_t : L.lltype =
    L.var_arg_function_type i32_t [||] in
  let printarray_func : L.llvalue =
      L.declare_function "printarray" printarray_t the_module in
  let fibonacci_t : L.lltype =
    L.var_arg_function_type i32_t [||] in
  let fibonacci_func : L.llvalue =
    L.declare_function "fibonacci" fibonacci_t the_module in
  let factorial_t : L.lltype =
    L.var_arg_function_type i32_t [||] in
  let factorial_func : L.llvalue =
    L.declare_function "factorial" factorial_t the_module in
  let touppercase_t : L.lltype =
    L.var_arg_function_type str_t [||] in
  let touppercase_func : L.llvalue =
    L.declare_function "touppercase" touppercase_t the_module in
  let tolowercase_t : L.lltype =
    L.var_arg_function_type str_t [||] in
  let tolowercase_func : L.llvalue =
    L.declare_function "tolowercase" tolowercase_t the_module in
  let reversestring_t : L.lltype =
    L.var_arg_function_type str_t [||] in
  let reversestring_func : L.llvalue =
    L.declare_function "reversestring" reversestring_t the_module in
  let randomnumber_t : L.lltype =
    L.var_arg_function_type i32_t (*replaced string_t*) [||] in
  let randomnumber_func : L.llvalue =
    L.declare_function "randomnumber" randomnumber_t the_module in
  let randomletter_t : L.lltype =
    L.var_arg_function_type str_t [||] in
  let randomletter_func : L.llvalue =
    L.declare_function "randomletter" randomletter_t the_module in
  let repeatstring_t : L.lltype =
    L.var_arg_function_type str_t [||] in
  let repeatstring_func : L.llvalue =
    L.declare_function "repeatstring" repeatstring_t the_module in
  let greatestcommondenominator_t : L.lltype =
    L.var_arg_function_type i32_t [||] in
  let greatestcommondenominator_func : L.llvalue =
    L.declare_function "greatestcommondenominator" greatestcommondenominator_t the_module in
  let range_t : L.lltype =
    L.var_arg_function_type (L.pointer_type i32_t) [||] in
  let range_func : L.llvalue =
    L.declare_function "range" range_t the_module in
  let rangewithstep_t : L.lltype =
    L.var_arg_function_type (L.pointer_type i32_t) [||] in
  let rangewithstep_func : L.llvalue =
    L.declare_function "rangewithstep" rangewithstep_t the_module in
  let isprime_t : L.lltype =
    L.var_arg_function_type i1_t [||] in
  let isprime_func : L.llvalue =
    L.declare_function "isprime" isprime_t the_module in
  let distance_t : L.lltype =
    L.var_arg_function_type i32_t [||] in
  let distance_func : L.llvalue =
    L.declare_function "distance" distance_t the_module in
  let concatenate_t : L.lltype =
    L.var_arg_function_type str_t [||] in
  let concatenate_func : L.llvalue =
    L.declare_function "concatenate" concatenate_t the_module in




  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl = (*build function body using the function declaration fdecl*)
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in
    let string_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

    let local_vars =
      let add_formal m (t, n) p =
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      (* declare local variables and allocate space for it *)
      and add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
      with Not_found -> StringMap.find n global_vars
    in

    let rec build_expr builder ((_, e) : sexpr) = match e with
        SIntegerLit i  -> L.const_int i32_t i
      | SBooleanLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SFloatLit l -> L.const_float_of_string float_t l
      | SNonePtr t -> L.const_pointer_null (ltype_of_typ t)
      | SStringLit s -> L.build_global_stringptr s "tmp" builder
      | SCharacterLit c -> L.const_int (ltype_of_typ A.Character) (Char.code c)
      | SId s       -> L.build_load (lookup s) s builder
      | SAssign (s, e) ->
        let e' = build_expr builder e in
        ignore(L.build_store e' (lookup s) builder); e'
      | SBinop (e1, op, e2) ->
        let e1' = build_expr builder e1
        and e2' = build_expr builder e2 in
        (match op with
          | A.Add     -> L.build_add
          | A.Sub     -> L.build_sub
          | A.Multiply -> L.build_mul
          | A.Divide -> L.build_sdiv
          | A.Modulus -> L.build_urem
          | A.And     -> L.build_and
          | A.Or      -> L.build_or
          | A.Equal   -> L.build_icmp L.Icmp.Eq
          | A.Neq     -> L.build_icmp L.Icmp.Ne
          | A.Less    -> L.build_icmp L.Icmp.Slt
          | A.Greater -> L.build_icmp L.Icmp.Sgt
          | A.LessOrEqual -> L.build_icmp L.Icmp.Sle
          | A.GreaterOrEqual -> L.build_icmp L.Icmp.Sge
        ) e1' e2' "tmp" builder
      | SUniop(op, ((t,_) as e)) -> (*Added 5-5-22*)
        let e' = build_expr builder e in
        (match op with
        | A.Not -> L.build_not) e' "tmp" builder
      | SCall ("print", [e]) ->
          L.build_call printf_func [| int_format_str ; (build_expr builder e) |] "printf" builder
      | SCall ("printfloat", [e]) ->
	        L.build_call printf_func [| float_format_str ; (build_expr builder e) |] "printf" builder
      | SCall ("printstring", [e]) ->
          L.build_call printf_func [| string_format_str ; (build_expr builder e) |] "printf" builder
      | SCall ("printboolean", [e]) ->
          L.build_call printboolean_func [| (build_expr builder e) |] "printboolean" builder
      | SCall ("printarray", [e1; e2]) ->
          L.build_call printarray_func [|  (build_expr builder e1); (build_expr builder e2)  |] "printarray" builder
      | SCall ("maximum", [e1; e2]) ->
          L.build_call maximum_func [| (build_expr builder e1); (build_expr builder e2)|] "maximum" builder
      | SCall ("minimum", [e1; e2]) ->
          L.build_call minimum_func [| (build_expr builder e1); (build_expr builder e2)|] "minimum" builder
      | SCall ("sum", [e1; e2]) ->
          L.build_call sum_func [| (build_expr builder e1); (build_expr builder e2) |] "sum" builder
      | SCall ("product", [e1; e2]) ->
          L.build_call product_func [| (build_expr builder e1); (build_expr builder e2) |] "product" builder
      | SCall ("reverse", [e1; e2]) ->
          L.build_call reverse_func [| (build_expr builder e1); (build_expr builder e2) |] "reverse" builder
      | SCall ("substring", [e1; e2; e3]) ->
          L.build_call substring_func
            [| (build_expr builder e1); (build_expr builder e2); (build_expr builder e3) |] "substring" builder
      | SCall ("replaceletters", [e1; e2; e3]) ->
          L.build_call replaceletters_func
            [| (build_expr builder e1); (build_expr builder e2); (build_expr builder e3) |] "replaceletters" builder
      | SCall ("scrambleletters", [e1]) ->
          L.build_call scrambleletters_func
            [| (build_expr builder e1) |] "scrambleletters" builder
      | SCall ("findchar", [e1; e2]) ->
          L.build_call findchar_func
	    [| (build_expr builder e1); (build_expr builder e2); |] "findchar" builder
      | SCall ("power", [e1; e2]) ->
          L.build_call power_func [| (build_expr builder e1); (build_expr builder e2); |] "power" builder
      | SCall ("absolute", [e1]) ->
          L.build_call absolute_func [| (build_expr builder e1); |] "absolute" builder
      | SCall ("fibonacci", [e1]) ->
          L.build_call fibonacci_func [| (build_expr builder e1); |] "fibonacci" builder
      | SCall ("factorial", [e1]) ->
          L.build_call factorial_func [| (build_expr builder e1); |] "factorial" builder
      | SCall ("touppercase", [e1]) ->
          L.build_call touppercase_func [| (build_expr builder e1); |] "touppercase" builder
      | SCall ("tolowercase", [e1]) ->
          L.build_call tolowercase_func [| (build_expr builder e1); |] "tolowercase" builder
      | SCall ("reversestring", [e1]) ->
          L.build_call reversestring_func [| (build_expr builder e1); |] "reversestring" builder
      | SCall ("randomnumber", [e1; e2]) ->
          L.build_call randomnumber_func [| (build_expr builder e1); (build_expr builder e2) |] "randomnumber" builder
      | SCall ("randomletter", []) ->
          L.build_call randomletter_func [| |] "randomletter" builder
      | SCall ("repeatstring", [e1; e2]) ->
          L.build_call repeatstring_func [| (build_expr builder e1); (build_expr builder e2) |] "repeatstring" builder
      | SCall ("greatestcommondenominator", [e1; e2]) ->
          L.build_call greatestcommondenominator_func
            [| (build_expr builder e1); (build_expr builder e2) |] "greatestcommondenominator" builder
      | SCall ("range", [e1; e2]) ->
          L.build_call range_func [| (build_expr builder e1); (build_expr builder e2) |] "range" builder
      | SCall ("rangewithstep", [e1; e2; e3]) ->
          L.build_call rangewithstep_func
            [| (build_expr builder e1); (build_expr builder e2); (build_expr builder e3) |] "rangewithstep" builder
      | SCall ("isprime", [e1]) ->
          L.build_call isprime_func [| (build_expr builder e1) |] "isprime" builder
      | SCall ("distance", [e1; e2; e3; e4]) ->
          L.build_call distance_func
          [| (build_expr builder e1); (build_expr builder e2); (build_expr builder e3); (build_expr builder e4) |] "distance" builder
      | SCall ("concatenate", [e1; e2]) ->
          L.build_call concatenate_func [| (build_expr builder e1); (build_expr builder e2) |] "concatenate" builder
      | SCall (f, args) ->
          let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (build_expr builder) (List.rev args) ) in
          (*llargs is in the original order*)
          let result = f ^ "_result" in
        L.build_call fdef (Array.of_list llargs) result builder
      | SArrayAccess(s, e) -> (* x[0]; *)
        (* Array reference: http://www.cs.columbia.edu/~sedwards/classes/2021/4115-spring/reports/JavaLite.pdf *)
          let (typ, _) = e in
          let position = L.build_add (build_expr builder e) (L.const_int i32_t 0) "position" builder in
          let elt = L.build_gep (build_expr builder (typ, (SId s))) [| position |] "eptr" builder in
          L.build_load elt "accelt" builder
      | SArrayLit(arr) -> (* [1,2,3,4,5]; *)
        (* Array reference: http://www.cs.columbia.edu/~sedwards/classes/2021/4115-spring/reports/JavaLite.pdf *)
        let (first, _) = List.hd arr in
        let typ = ltype_of_typ (A.Arr(first, (List.length arr))) in
        let arrayalloc = L.build_array_alloca typ (L.const_int i32_t ((List.length arr) + 1)) "arr" builder in
        let aptr = L.build_pointercast arrayalloc typ "aptr" builder in
        let store_elt ind elt =
          let position = L.const_int i32_t (ind) in
        let eptr = L.build_gep aptr [| position |] "aelt" builder in
          ignore(L.build_store elt eptr builder) in
        List.iteri store_elt (List.map (build_expr builder) arr);
        let eptr = L.build_gep aptr [| (L.const_int i32_t (List.length arr)) |] "l" builder in
        ignore(L.build_store (L.const_null (L.element_type typ)) eptr builder);
        aptr
      | SArrAssign(s, e1, e2) -> (* array[0] = 5; *)
          (* Array reference: http://www.cs.columbia.edu/~sedwards/classes/2021/4115-spring/reports/JavaLite.pdf *)
          let (typ, _) = e1 in
          let position = L.build_add (build_expr builder e1) (L.const_int i32_t 0) "position" builder in
          let eptr = L.build_gep (build_expr builder (typ, (SId s))) [| position |] "aelt" builder in
          L.build_store (build_expr builder e2) eptr builder
      | SAssignToVar(var, typ, e) -> (* x integer = 5; *)
          build_expr builder e(*temporary*)
      (* | SNonePtr typ -> build_expr builder typ *)
    in

    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (*this tells the program whether a then/else block contains a jump or terminating statement*)
         (L.insertion_block builder) with
        Some _ -> () (*does not insert instruction instr into the program*)
      | None -> ignore (instr builder) in (*if it does not have a terminator, insert instruction instr into the program*)

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    let rec build_stmt builder (*pointer to insert the next instruction*) = function
        SBlock sl -> List.fold_left build_stmt builder sl
      | SExpr e -> ignore(build_expr builder e); builder
      | SReturn e ->
        ignore(L.build_ret (build_expr builder e) builder); builder
      | SIf(predicate, then_stmt) ->
        let bool_val = build_expr builder predicate in

        let then_bb = L.append_block context "then" the_function in
        ignore (build_stmt (L.builder_at_end context then_bb) then_stmt);

        let end_bb = L.append_block context "if_end" the_function in
        let build_br_end = L.build_br end_bb in
        add_terminal (L.builder_at_end context then_bb) build_br_end;

        ignore(L.build_cond_br bool_val then_bb end_bb builder);
        L.builder_at_end context end_bb
      | SIfElse (predicate, then_stmt, else_stmt) ->
        let bool_val = build_expr builder predicate in

        let then_bb = L.append_block context "then" the_function in
        ignore (build_stmt (L.builder_at_end context then_bb) then_stmt);
        let else_bb = L.append_block context "else" the_function in
        ignore (build_stmt (L.builder_at_end context else_bb) else_stmt);

        let end_bb = L.append_block context "if_end" the_function in
        let build_br_end = L.build_br end_bb in
        add_terminal (L.builder_at_end context then_bb) build_br_end;
        add_terminal (L.builder_at_end context else_bb) build_br_end;

        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context end_bb

      | SWhile (predicate, body) ->

        let while_bb = L.append_block context "while" the_function in
        let build_br_while = L.build_br while_bb in
        ignore (build_br_while builder);
        let while_builder = L.builder_at_end context while_bb in
        let bool_val = build_expr while_builder predicate in

        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (build_stmt (L.builder_at_end context body_bb) body) build_br_while;

        let end_bb = L.append_block context "while_end" the_function in

        ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
        L.builder_at_end context end_bb (*final return value - every prior return has to be ignored *)

      | SFor (e1, e2, e3, body) ->

	let e1 = SExpr e1 in build_stmt builder
	( SBlock [e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
    in
    (* Build the code for each statement in the function *)
    let func_builder = build_stmt builder (SBlock fdecl.sbody) in

(*this code is used for IR generation*)

    (* Add a return if the last block falls off the end *)
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0))
(*if you do not add a return instruction, you will always return 0*)

  in

  List.iter build_function_body functions;
  the_module (*module generates IR code, context stores the data you will need during compilation*)
(*always generate code to the module*)
(*functions is a list of function definitions.
in sast.ml, see type sprogram = bind list * sfunc_def list*)
