(* Melvin Philips, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option(s:string , xs: string list)=
    let
	fun is_contain(remlst: string list)=
	    case remlst of
		[]=>false
	     |r::r' => if same_string(s,r) then true else is_contain(r')
	fun calc(xs1:string list, acc:string list)=
	    case xs1 of
		[]=> SOME acc
		|x::xs1'  => if same_string(x,s) then calc(xs1',acc) else calc(xs1',acc@[x])  
    in 

       case xs of
	   []=> NONE
        |ret  => if is_contain(xs) then calc(xs,[]) else NONE  
    end
 
   
fun get_substitutions1(xss:string list list, s :string)=
    let 
	fun is_contain(remlst: string list)=
	    case remlst of
		[]=>false
	     |r::r' => if same_string(s,r) then true else is_contain(r') 
    in
	case xss of
	    [] => []
	   |x::xss'  =>  if is_contain(x)  
			 then case all_except_option(s,x) of
				  NONE => get_substitutions1(xss', s)
				| SOME str => str @ get_substitutions1(xss', s)
			 else get_substitutions1(xss',s)
     end                                    


fun get_substitutions2(xss:string list list, s :string)=
    let
	fun get_inner(xss:string list list,acc: string list)=
	    let 
		fun is_contain(remlst: string list)=
		    case remlst of
			[]=>false
		       |r::r' => if same_string(s,r) then true else is_contain(r')
	    in
		case xss of
		    [] => acc
		   |x::xss'  =>  if is_contain(x)  
				 then case all_except_option(s,x) of
					  NONE => get_inner(xss',acc)
					| SOME str => get_inner(xss', acc@str)
				 else get_inner(xss',acc)
	   end
     in
         get_inner(xss,[])
    end                                    

fun similar_names(subs, {first= f, middle= m, last= l}) =
    let
        val full_n = {first= f, middle= m, last= l}
        fun generate_subs(substitutions: string list) =
            case substitutions of
                [] => []
             | x::xs => {first= x, middle= m, last= l} :: generate_subs(xs)
    in
        [full_n] @ generate_subs(get_substitutions2(subs, f))
    end


 (* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 




exception IllegalMove

(* put your solutions for problem 2 here *)


fun card_color(c) =
    case c of
        (Spades, _) => Black
      | (Clubs, _) => Black
      | (_, _) => Red

fun card_value(c) =
    case c of
        (_, King) => 10
      | (_, Queen) => 10
      | (_, Jack) => 10
      | (_, Ace) => 11
      | (_, Num value) => value


fun remove_card(cs,c,e)=
    let
	fun same_card(s1, s2) =
	    s1 = s2

	fun is_contain(remlst)=
		    case remlst of
			[]=>false
		       |r::r' => if same_card(c,r) then true else is_contain(r')
	fun calc(xs1, acc)=
	    case xs1 of
		[]=> acc
		|x::xs1'  => if same_card(x,c) 
			     then xs1'@acc 
			     else calc(xs1',acc@[x])
        
    in 
	if is_contain(cs)
	then	     
	    case cs of
		[]=> raise e
               |ret  => calc(cs,[])
	else
	    raise e
    end

fun all_same_color(cs:card list) =
    case cs of
           [] => true
	   | x::[] =>true   
	   | x::x1::xs => card_color(x) = card_color(x1) andalso all_same_color(x1::xs)
     
fun sum_cards(cs:card list) = 
    let
        fun calc_sum(cs, total) =
            case cs of
                [] => total
              | x::xs => calc_sum(xs, total + card_value(x))
    in
        calc_sum(cs, 0)
    end

fun score(cs:card list,goal:int)= 
    let 
	val total_score=sum_cards(cs)
        val pscore= if total_score > goal then 3 * (total_score - goal) else (goal - total_score)
    in 
	if(all_same_color(cs)) then pscore div 2 else pscore
    end

fun officiate(cs :card list, mv: move list ,goal:int)=
    let
	fun go_play(cs1:card list, mv1 : move list, hc: card list)=
	    case mv1 of
		[]=>hc
		  |Discard c::m'  => go_play(cs1,m',remove_card(hc,c,IllegalMove))
		  |Draw ::m' => case cs1 of 
				  []=>hc
				    | c::c' => if (sum_cards(hc)+card_value(c) > goal)
						then c::hc
					        else go_play(c',m',c::hc)
	  
  
    in
	score(go_play(cs,mv,[]),goal)
    end
