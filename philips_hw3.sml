(* Coursera Programming Languages, Homework 3, Provided Code *)



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



fun only_capitals xs =
    List.filter (fn x => Char.isUpper (String.sub( x, 0)))xs
(*
fun f (x,y)=
    if String.size x > String.size y then x else y
*)
fun longest_string1(xs)=
    List.foldl (fn (x,y)=> if String.size x > String.size y then x else y) "" xs

fun longest_string2(xs)=
    List.foldl (fn (x,y)=> if String.size x >= String.size y then x else y) "" xs

fun longest_string_helper f xs=
    List.foldl (fn (x,y) =>if f(String.size x, String.size y) then x else y) "" xs

val longest_string3 = longest_string_helper(fn (x,y)=> x > y) 

val longest_string4 = longest_string_helper(fn (x,y ) =>x >=y )

val longest_capitalized = longest_string1 o only_capitals 

val rev_string = String.implode o List.rev o String.explode

exception NoAnswer

fun first_answer f xs =
    case xs  of
	[] => raise NoAnswer
     | x::xs' => case f(x) of
		     NONE => first_answer f xs'
		  | SOME v => v  

fun all_answers f xs =
    let
	fun all_answers_helper acc xs1=
	    case xs1 of
	      []=>  SOME acc 
	     |x::xs' => case f(x) of  
			    NONE =>NONE
			    |SOME v => all_answers_helper (v@acc) xs'
    in 
	all_answers_helper [] xs
    end