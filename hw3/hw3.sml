(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1 *)

fun only_capitals ls = List.filter (fn s => Char.isUpper(String.sub(s,0))) ls

(* Test *)
val t11 = only_capitals ["Aw", "aa", "bb", "Bb", "CCC", "qwe"]

(* 2 *)

fun longest_string1 ls = List.foldl (fn (x,y) => if String.size x > String.size y 
						 then x
						 else y) "" ls

(* Test *)
val t21 = longest_string1 ["Aw", "aa", "bb", "Bb", "CCC", "qwe"]
val t22 = longest_string1 ["Aw", "aa", "bb", "Bbwww", "CCC", "qwe"]
val t23 = longest_string1 ["Aw", "aa", "bb", "Bb", "CCwC", "qwe"]
val t24 = longest_string1 []

(* 3 *)

fun longest_string2 ls = List.foldl (fn (x,y) => if String.size x >= String.size y 
						 then x
						 else y) "" ls

(* Test *)
val t31 = longest_string2 ["Aw", "aa", "bb", "Bb", "CCC", "qwe"]
val t32 = longest_string2 ["Aw", "aa", "bb", "Bbwww", "CCC", "qwe"]
val t33 = longest_string2 ["Aw", "aa", "bb", "Bb", "CCwC", "qwe"]
val t34 = longest_string2 []

(* 4 *)

fun longest_string_helper f ls = List.foldl (fn (x,y) => if f(String.size(x),String.size(y)) then x else y) "" ls

val longest_string3 = longest_string_helper (fn (x,y) => x > y)

(* Test *)
val t41 = longest_string3 ["Aw", "aa", "bb", "Bb", "CCC", "qwe"]
val t42 = longest_string3 ["Aw", "aa", "bb", "Bbwww", "CCC", "qwe"]

val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

(* Test *)
val t43 = longest_string4 ["Aw", "aa", "bb", "Bb", "CCC", "qwe"]
val t42 = longest_string4 ["Aw", "aa", "bb", "Bbwww", "CCC", "qwe"]

(* 5 *)

val longest_capitalized = longest_string1 o only_capitals

(* Test *)
val t51 = longest_capitalized ["Aw", "aa", "bb", "Bb", "C", "qwe"]
val t52 = longest_capitalized ["Aw", "aakkkkk", "bb", "Bbwww", "CCC", "qwe"]
val t53 = longest_capitalized ["Aw", "aa", "bb", "Bbkj", "CCwC", "qwoioo"]
val t54 = longest_capitalized []

(* 6 *)

val rev_string = String.implode o List.rev o String.explode

(* Test *)
val t61 = rev_string "Georgios_Kyritsis"

(* 7 *)

fun first_answer f ls = 
  case ls of
      [] => raise NoAnswer
   | x::xs => case f x of
		  NONE => first_answer f xs
	       | SOME v => v  

(* 8 *)

fun all_answers f ls =
  let
      fun aux(lista, acc) =
	case lista of
	    [] => acc
	 | x::xs => case f x of
			NONE => []
		     | SOME i => aux(xs, acc @ i)
  in
      case aux(ls, []) of
	  [] => NONE
       | x::xs => SOME(x::xs)
  end

(* 9b *)

fun count_wildcards p = g (fn x => 1) (fn y => 0) p

(* Test *)
val count_wildcards_test1 = count_wildcards Wildcard = 1
val count_wildcards_test2 = count_wildcards (Variable "hi") = 0
val count_wildcards_test3 = count_wildcards (TupleP [Variable "hi", Wildcard, Variable "ha", Wildcard]) = 2
val count_wildcards_test4 = count_wildcards (ConstructorP("hi", TupleP[Wildcard, Variable "ha"])) = 1

(* 9c *)

fun count_wild_and_variable_lengths p = g (fn x => 1) (String.size) p

(* Test *)
val count_wild_and_variable_lengths_test1 = count_wild_and_variable_lengths Wildcard = 1
val count_wild_and_variable_lengths_test2 = count_wild_and_variable_lengths (Variable "hi") = 2
val count_wild_and_variable_lengths_test3 = count_wild_and_variable_lengths UnitP = 0
val count_wild_and_variable_lengths_test4 = count_wild_and_variable_lengths (ConstructorP("hi", TupleP[Wildcard, Variable "ha"])) = 3

(* 9d *)

fun count_some_var (s, p) = g (fn x => 0) (fn y => if y = s then 1 else 0) p


(* Test *)
val count_some_var_test1 = count_some_var("hi", Wildcard) = 0
val count_some_var_test2 = count_some_var("hi", (Variable "hi")) = 1
val count_some_var_test3 = count_some_var("hi", UnitP) = 0
val count_some_var_test4 = count_some_var("hi", TupleP[Wildcard, Variable "hi", Variable "ha"]) = 1
val count_some_var_test5 = count_some_var("hi", ConstructorP("hi", TupleP[Wildcard, Variable "hi", TupleP[Variable "hi", Variable "ho"]])) = 2

(* 10 *)

(* helper function *)
fun helper1 p = 
  case p of
      Wildcard => []
   | Variable i => [i]
   | TupleP pl => List.foldl (fn (x,y) => ((helper1 x) @ y)) [] pl
   | ConstructorP (_,p) => helper1 p
   | _  => []

(* helper function *)
fun helper2 s = 
  case s of
      [] => true
   | x::xs => if List.exists (fn y => y = x) xs then false else helper2 xs

fun check_pat p = helper2 (helper1 p)  

(* Test *)
val check_pat_test1 = check_pat Wildcard = true
val check_pat_test2 = check_pat (Variable "hi") = true
val check_pat_test3 = check_pat (TupleP[Wildcard, Variable "hi", ConstructorP("hi", Variable "ho")]) = true
val check_pat_test4 = check_pat (TupleP[Wildcard, Variable "hi", ConstructorP("ho", Variable "hi")]) = false

(* 11 *)

fun match (v, p) = 
  case (v,p) of
      (_, Wildcard) => SOME []
   | (Const i1, ConstP i2) => if i1 = i2 then SOME [] else NONE
   | (i1, Variable i2) => SOME [(i2,i1)]
   | (Unit, UnitP) => SOME []
   | (Tuple i1, TupleP i2) => if List.length i1 = List.length i2
			      then all_answers match (ListPair.zip(i1,i2))
			      else NONE
   | (Constructor (s1, v1), ConstructorP (s2, p2)) => if s1 = s2 then (match (v1, p2)) else NONE
   | _ => NONE  
    
(* 12 *)

fun first_match (v, ps) = 
  SOME (first_answer (fn p => match(v,p)) ps) handle NoAnswer => NONE
