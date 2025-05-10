
open Types
open Unification
open Operator

exception Not_a_number
exception Cant_evaluate


(* gets list of variables in term *)
let rec get_variables term list =
  let rec get_vars_from_args args list = (* gets variables from arguments *)
	match args with
	| [] -> list
	| t :: terms -> 
	  list |> get_variables t |> get_vars_from_args terms
  in
	match term with
    | TermBinOp (_, t1, t2) ->
	  list |> get_variables t1 |> get_variables t2
	| TermVariable v ->  (* is this in List.??? *)
	  if List.exists (fun var -> var = v) list 
	  then list 
	  else v :: list
	| TermFunctor (_, args) -> get_vars_from_args args list
	| TermNegation t -> get_variables t list
	| TermList listterm -> listterm |> (function
	  | EmptyList -> list
	  | NormalList args -> get_vars_from_args args list
	  | DividedList (args, term) -> 
		list |> get_vars_from_args args |> get_variables term)
	| _ -> list

let map_uniques = List.map (fun var -> (var, TermVariable (Var.get_unique ())))

(* makes variables in clause unique *)
let make_unique = function
  | SingleClause term -> 
	SingleClause (get_variables term [] |> map_uniques |> replace term)
  | ClauseImplication (term1, term2) ->
	let replacement = 
	  get_variables term1 [] |> get_variables term2 |> map_uniques
	in
	  ClauseImplication (replace term1 replacement,
						 replace term2 replacement)

(* evaluates arithmetic expression *)
let rec apply_arith_operator t1 t2 f =
  let n1 = arith_eval t1
  and n2 = arith_eval t2
  in match n1, n2 with
  | Integer i1, Integer i2 -> Integer (f i1 i2)
and arith_eval = function
  | TermConstant (ConstantNumber n) -> n
  | TermConstant _ -> raise Not_a_number
  | TermBinOp (op, t1, t2) ->
	function_of_operation op |> apply_arith_operator t1 t2
  | _ -> raise Not_a_number
			
let maybe_shuffle randomise clauses = 
  if randomise 
  then Shuffle.shuffle clauses 
  else clauses

(* evaluates functor 
functor_term is a term to evaluate
database is a database loaded into the program 
rep is a replacement
clauses is a list of clauses from database that haven't beed checked yet
cont is a continuation *)

(* 
sc: success callback
fc: failure callback
*)

let evaluate term database rep clauses sc fc cut_c randomise =
  let maybe_shuffle = maybe_shuffle randomise in

  let rec functor_eval functor_term database rep clauses sc fc cut_c =
	let term = replace functor_term rep in (* replace variables in term *)
	let eval_clause clauses' = function
	  | SingleClause dterm -> 
			    (* found a fact in database *)
		let uni = (unify term dterm rep)
		in
		  (* term unifies with fact in database, 
			 so store rest of possible calculations 
			 and return result of unification *)
		  (* otherwise, it didn't unify => try another possibilities *)
		  if fst uni 
		  then sc uni (fun() -> 
			functor_eval term database rep clauses' sc fc cut_c) 
		  else functor_eval term database rep clauses' sc fc cut_c 
	  | ClauseImplication (dterm, condition) ->
		(* found an implication in database, 
		   try to unificate with its result (left side term) *)
		let uni = (unify term dterm rep) 
		in		 
		  if fst uni 
		  then evaluate condition database (snd uni) database 
			(fun vt fc' -> sc vt fc') 
			(fun () -> 
			  functor_eval term database rep clauses' sc fc cut_c) 
			fc
		  else functor_eval term database rep clauses' sc fc cut_c
	in
	  match clauses with
	  | [] -> 
		(* no more facts or implications in database *)
		sc (false,[]) fc 
	  | dclause :: clauses'' ->
		let clauses' = maybe_shuffle clauses''
		in
		  eval_clause clauses' (make_unique dclause)
							
  (* evaluates terms *)
  and evaluate term database rep clauses sc fc cut_c =
	let arith_comparison t1 t2 f =
	  let n1 = arith_eval t1
	  and n2 = arith_eval t2
	  in
		(match n1, n2 with
		| Integer i1, Integer i2 -> sc ((f i1 i2), rep) fc)
	and arith_equality t1 t2 flag =
	  let n1 = arith_eval t1
	  and n2 = arith_eval t2
	  in
		if (n1 = n2) = flag
		then sc (true, rep) fc
		else sc (false, []) fc
	in

	let repterm = replace term rep (* apply replacement to the term *)
	in
	let evaluate_binary_operation = function
	  | TermTermUnify, term1, term2 -> sc (unify term1 term2 rep) fc
	  | TermTermNotUnify, term1, term2 -> 
		let uni = unify term1 term2 rep in
		  sc (not (fst uni), snd uni) fc
	  | TermArithmeticEquality, t1, t2 -> arith_equality t1 t2 true
	  | TermArithmeticInequality, t1, t2 -> arith_equality t1 t2 false
	  | TermArithmeticLess, t1, t2 -> arith_comparison t1 t2 (<)
	  | TermArithmeticGreater, t1, t2 -> arith_comparison t1 t2 (>)
	  | TermArithmeticLeq, t1, t2 -> arith_comparison t1 t2 (<=)
	  | TermArithmeticGeq, t1, t2 -> arith_comparison t1 t2 (>=)
	  | TermTermEquality, t1, t2 -> sc (t1 = t2,rep) fc
	  | TermIs, t1, t2 -> 
		let n2 = TermConstant (ConstantNumber (arith_eval t2))
		in
		  sc (unify t1 n2 []) fc
	  | TermAnd, t1, t2 -> 
		evaluate t1 database rep clauses (* evaluate first term *)
		  (fun vt1 fc1 ->
			if fst vt1 then
			  evaluate t2 database (snd vt1) clauses
				(fun vt2 fc2 -> sc vt2 fc2) fc1 cut_c 
			else sc (false,[]) fc1) fc cut_c
	  | TermOr, t1, t2 -> evaluate t1 database rep clauses
		(fun vt fc' -> sc vt fc')
		(fun () -> 
		  evaluate t2 database rep clauses sc fc cut_c) cut_c
	  | _ -> raise Cant_evaluate
	in
	  match repterm with
	  | TermBinOp (op, term1, term2) -> evaluate_binary_operation (op, term1, term2)
	  | TermNegation t ->
		evaluate t database rep clauses
		  (fun vt fc' -> sc (not (fst vt), snd vt) fc') fc cut_c
	  | TermFunctor(_,_) ->
		functor_eval repterm database rep clauses sc fc cut_c
	  | TermCut -> sc (true,rep) cut_c
	  | _ -> raise Cant_evaluate
  in
	evaluate term database rep clauses sc fc cut_c
