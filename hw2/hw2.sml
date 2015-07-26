(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(str: string, lst: string list)=
  let 
    fun remove(s: string, l: string list)=
      case l of
	  [] => []
         | x::xs' => if same_string(s,x)
		     then xs'
		     else x::remove(s,xs')
    val result = remove(str,lst)
  in 
      if result = lst
      then NONE 
      else SOME result
  end

(* Test *)
val all_except_option1 = all_except_option("a", ["b","c","a","aa","k"])
val all_except_option2 = all_except_option("a", ["b","c","d"])


fun get_substitutions1(substitutions: string list list, s: string)=
  case substitutions of
      [] => []
	 | x::xs' => let
	                 val res = all_except_option(s,x)
		     in
			 case res of
			     NONE => get_substitutions1(xs',s)
				  | SOME i => i @ get_substitutions1(xs',s)
		     end

(* Test *)
val get_substitutions1_1 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred")

fun get_substitutions2(substitutions: string list list, s: string)=
  let
      fun aux(subs: string list list, acc: string list)=
	case subs of
	    [] => acc
	       | x::xs' => case all_except_option(s,x) of
			       NONE => aux(xs',acc)
				    | SOME i => aux(xs', i @ acc) 

  in
      aux(substitutions, [])
  end

(* Test *)
val get_substitutions2_1 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred")


fun similar_names(substitutions: string list list, {first=f, middle=m, last=l})=
  let
      val names = f::get_substitutions2(substitutions,f)
      fun aux(first_name: string list)=
	case first_name of
	    [] => []
	       | x::xs' => {first=x, middle=m, last=l} :: aux(xs') 
  in
      aux(names)
  end

(* Test *)
val similar_names1 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"})

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(c: card)=
  case c of
      (Clubs, _) => Black
   | (Spades, _) => Black
   | _ => Red  

(* Tests *)
val card_color1 = card_color(Clubs, Ace)
val card_color2 = card_color(Diamonds, King)
val card_color3 = card_color(Hearts, Queen)


fun card_value(c: card)=
  case c of
      (_, Ace) => 11
   | (_, Num i) => i
   | (_) => 10

(* Tests *)
val card_value1 = card_value(Clubs, Ace)
val card_value2 = card_value(Diamonds, Num 10)
val card_value3 = card_value(Clubs, Queen)
val card_value4 = card_value(Spades, Num 9)


fun remove_card(cs: card list, c: card, e: exn)=
  let fun remove(clist: card list)=
	case clist of
	    [] => []
	 | x::xs' => if x=c
		     then xs'
		     else x::remove(xs')
      val res = remove(cs)
  in
      if res = cs then raise e else res
  end

(* Tests *)
val remove_card1 = remove_card([(Clubs,Ace), (Diamonds,King), (Spades, Num 9)], (Clubs, Ace), IllegalMove)  


fun all_same_color(cs: card list)=
  case cs of
      [] => true
   | x::[] => true
   | x::neck::tail => if card_color(x) = card_color(neck) 
		      then all_same_color(neck::tail)
		      else false

(* Tests *)
val all_same_color1 = all_same_color([(Clubs,Ace), (Diamonds,King), (Spades, Num 9)])
val all_same_color2 = all_same_color([(Clubs,Ace), (Spades, Num 9)])

fun sum_cards(cs: card list)=
  let
    fun aux(cl: card list, acc: int)=
      case cl of
	  [] => acc
	| x::xs' => aux(xs', card_value(x)+acc)
  in
    aux(cs, 0)
end

(* Tests *)
val sum_cards1 = sum_cards([(Clubs,Ace), (Diamonds,King), (Spades, Num 9)])
val sum_cards2 = sum_cards([(Clubs,Ace), (Spades, Num 9)])


fun score(cs: card list, goal: int)=
  let 
      val sum = sum_cards(cs)
      val prel = if sum - goal > 0 then 3*(sum-goal) else goal-sum
      val col = all_same_color(cs)
  in
      if col then prel div 2 else prel
  end

(* Tests *)
val score1 = score([(Clubs,Ace), (Diamonds,King), (Spades, Num 9)], 45)


fun officiate(cs: card list, mv: move list, goal: int)=
  let
      fun aux(held_cards: card list, table_cards: card list, moves: move list)=
	case moves of
	    [] => score(held_cards, goal)
	 | x::xs' => case x of
			 Discard c => aux(remove_card(held_cards, c, IllegalMove), table_cards, xs')
		      | Draw => case table_cards of
				    [] => score(held_cards, goal)
				 | y::ys' => if sum_cards(y::held_cards) > goal
					     then score(y::held_cards, goal)
					     else aux(y::held_cards,ys',xs')
  in
      aux([], cs, mv)
  end


(* helper function *)
fun ace_counter(cs: card list)=
  case cs of
      [] => 0
   | x::xs' => if card_value(x) = 11
	       then 1 + ace_counter(xs')
	       else ace_counter(xs')


(* Test *)
val ace_counter1 = ace_counter([(Clubs,Ace), (Diamonds,King), (Spades, Num 9)])
val ace_counter2 = ace_counter([(Clubs,Ace), (Spades, Num 9), (Diamonds, Ace)])
val ace_counter3 = ace_counter([(Clubs,Queen), (Spades, Num 9), (Diamonds, King)])

(* helper function *)
fun all_sums_aces(sum: int, number_of_aces: int)=
  if number_of_aces = 0 
  then [sum]
  else sum::all_sums_aces(sum-10, number_of_aces-1)

(* Test *)
val all_sums_aces1 = all_sums_aces(52,3)


(* helper function *)    
fun all_possible_sums(cs: card list)=
  all_sums_aces(sum_cards(cs), ace_counter(cs))


(* Test *)
val all_possible_sums1 = all_possible_sums([(Clubs,Ace), (Diamonds,King), (Spades, Num 9)])
val all_possible_sums2 = all_possible_sums([(Clubs,Ace), (Spades, Num 9), (Diamonds, Ace)])
val all_possible_sums3 = all_possible_sums([(Clubs,Queen), (Spades, Num 9), (Diamonds, King)])		
	 
(* helper function *)
fun min_list(l: int list)=
  case l of
      [] => raise List.Empty
   | x::[] => x
   | x::xs' => Int.min(x, min_list(xs'))


(* Test *)
val min_list1 = min_list([1,2,3,4])
val min_list2 = min_list([33,22,44,11,99]) 


(* helper *)
fun score(sum: int, goal: int, cs: card list)=
  let 
      val prel = if sum - goal > 0 then 3*(sum-goal) else goal-sum
      val col = all_same_color(cs)
  in
      if col then prel div 2 else prel
  end


(* helper function *)
fun map(f, xs)=
  case xs of 
      [] => []
	 | x::xs' => f(x) :: map(f, xs') 


fun score_challenge(cs: card list, goal: int)=
  let 
      val all_scores = all_possible_sums(cs)
      fun score(sum: int)=
	let 
	    val prel = if sum - goal > 0 then 3*(sum-goal) else goal-sum
	    val col = all_same_color(cs)
	in
	    if col then prel div 2 else prel
	end
  in    
      min_list(map(fn x => score(x), all_scores))
end

val score_challenge1 = score_challenge([(Diamonds, Ace), (Hearts, Num 9), (Clubs, Ace), (Spades, Num 4)], 13) = 6


fun officiate_challenge(cs: card list, mv: move list, goal: int)=
  let
      fun aux(held_cards: card list, table_cards: card list, moves: move list)=
	case moves of
	    [] => score_challenge(held_cards, goal)
	 | x::xs' => case x of
			 Discard c => aux(remove_card(held_cards, c, IllegalMove), table_cards, xs')
		      | Draw => case table_cards of
				    [] => score_challenge(held_cards, goal)
				 | y::ys' => if min_list(all_possible_sums(y::held_cards)) > goal
					     then score_challenge(y::held_cards, goal)
					     else aux(y::held_cards,ys',xs')
  in
      aux([], cs, mv)
  end


