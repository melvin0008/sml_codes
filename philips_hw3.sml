(*Melvin Philips Coursera Programming Languages, Homework 3, Provided Code *)



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


(*1*)
fun only_capitals xs =
    List.filter (fn x => Char.isUpper (String.sub( x, 0)))xs
(*2*)
fun longest_string1(xs)=
    List.foldl (fn (x,y)=> if String.size x > String.size y then x else y) "" xs
(*3*)
fun longest_string2(xs)=
    List.foldl (fn (x,y)=> if String.size x >= String.size y then x else y) "" xs

(*helper for 4 & 5*)
fun longest_string_helper f xs=
    List.foldl (fn (x,y) =>if f(String.size x, String.size y) then x else y) "" xs

(*4 a*)
val longest_string3 = longest_string_helper(fn (x,y)=> x > y) 

(*4 b*)
val longest_string4 = longest_string_helper(fn (x,y ) =>x >=y )

(*5*)
val longest_capitalized = longest_string1 o only_capitals 

(*6*)
val rev_string = String.implode o List.rev o String.explode

(*7*)
exception NoAnswer

fun first_answer f xs =
    case xs  of
	[] => raise NoAnswer
     | x::xs' => case f(x) of
		     NONE => first_answer f xs'
		  | SOME v => v  

(*8*)
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

(*9 a*)
fun count_wildcards p =
    g (fn x => 1) (fn x => 0) p
(*9 b*)
fun count_wild_and_variable_lengths p =
    g (fn x => 1) (fn x => String.size(x)) p
(* 9 c*)
fun count_some_var (s : string, p : pattern) =
    g (fn x => 0) (fn x => if x = s then 1 else 0) p

(*10*)
fun check_pat p=
    let fun get_all_strings pat =
	    case pat of 
	      Wildcard =>[]
	     |Variable x =>[x] 
	     |TupleP xs' => List.foldl (fn (pat, i)=>get_all_strings pat @i)[] xs'
	     |ConstructorP(_, p) => get_all_strings p 
	     | _ =>[] 
	fun get_distinct (lst)=
	      case lst of
              [] => true
              | x :: x' => if List.exists (fn y => y = x) x' then false else get_distinct x'
    in get_distinct (get_all_strings (p))       
    end
(*11*)
fun match (v : valu, p : pattern) =
    case p of
        Wildcard => SOME []
      | Variable s => SOME [(s, v)]
      | UnitP => (case v of 
                     Unit => SOME [] 
                   | _ => NONE)
      | ConstP i => (case v of 
                        Const j => if i = j then SOME [] else NONE
                      | _ => NONE)
      | TupleP l1 => (case v of
                      Tuple l2 => (all_answers (fn (x,y) => match(x,y)) (ListPair.zipEq(l2, l1)) handle UnequalLengths => NONE)
                                  | _ => NONE)
      | ConstructorP (s, pat) => (case v of
                                 Constructor (va, b) => if s =va then match(b, pat) else NONE
                                   | _ =>NONE) 
											    
(*12*)                             
fun first_match a lst =
    SOME (first_answer (fn x => match(a, x)) lst) 
                                      handle NoAnswer => NONE


