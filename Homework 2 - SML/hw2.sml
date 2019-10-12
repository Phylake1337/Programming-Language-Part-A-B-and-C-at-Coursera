fun same_string(s1 : string, s2 : string) =
    s1 = s2

(*Problem 1*)
fun all_except_option(s, stringList)=
    case stringList of
	[] => NONE
      | x::[] => if same_string(s,x) then SOME [] else NONE 
      | x::xs' =>
	let
	    fun Check(x,xs')=
		case xs' of
		    [] => if same_string(s,x) then [] else [x]
		  | y::ys' => if same_string(s,x) then xs' else x::Check(y,ys')
        in
	    let
		val outList = Check(x,xs')
	    in
		if outList = stringList then NONE else SOME outList
	    end
        end

	    

fun get_substitutions1(subs,s)=
    case subs of
	[]=> []
      | x::xs' => case all_except_option(s,x) of
		      NONE => get_substitutions1(xs',s)
		    | SOME L => L@ get_substitutions1(xs',s)


fun get_substitutions2(subs,s)=
    let
	fun help_subs(subs,accList)=
	    case subs of
		[] => accList
	      | x::xs' => case all_except_option(s,x) of
			      NONE => help_subs(xs',accList)
			    | SOME L => help_subs(xs',L@accList)
    in
	help_subs(subs,[])
    end

			

fun similar_names(subs, {first=first, middle=middle, last=last})=
    let
	val first_names = get_substitutions2(subs,first);
	fun generate_names(first_names)=
	    case first_names of
		[] => []
	      | x::xs' => {first=x, middle=middle, last=last}::generate_names(xs')
    in
	{first = first, middle = middle, last = last}::generate_names(first_names)
    end

(*Problem 2*)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color(card)=
    case card of
	(Clubs,_) => Black
       |(Diamonds,_) => Red
       |(Hearts,_) => Red
       |(Spades,_) => Black

fun card_value(card)=
    case card of
	(_, Num i) => i
      | (_, Ace)  => 11
      | (_, Queen) => 10
      | (_, Jack) => 10
      | (_, King) => 10

fun remove_card(cs, c, e)=
    case cs of
	[] => raise e
      | x::[] => if c = x then [] else raise e 
      | x::xs' =>
	let
	    fun Check(x,xs')=
		case xs' of
		    [] => if c = x  then [] else [x]
		  | y::ys' => if c = x then xs' else x::Check(y,ys')    
	in
	    let
		val outList = Check(x,xs')
	    in
		if outList = cs then raise e else outList end
	end
	    

fun all_same_color(cardList)=
    case cardList of
	[] => true
      | [x1] => true
      | x1::(x2::xs') =>if card_color(x1) = card_color (x2) then all_same_color((x2::xs'))
			else false



fun sum_cards(cardList)=
    let
	fun sum_help(cards, acc)=
	    case cards of
		[] => acc
	      | x::xs' => sum_help(xs', acc + card_value(x)) 	    
    in
	sum_help(cardList, 0)
    end
	
fun score(cardList, goal)=
    let
	fun compare()=	    
	    if sum_cards(cardList) > goal then 3*(sum_cards(cardList) - goal)
	    else goal - sum_cards(cardList)
    in
	if all_same_color(cardList) then compare() div 2 else compare()
    end
	
fun draw_card(cardList,heldList)=
    case cardList of
	[] => ([],heldList)
      | x::xs' =>(xs',  x::heldList)


	     
fun officiate(cardList, moves, goal)=
    let		
	fun current_state(cardList, heldList, moves, flag)=
	    if sum_cards(heldList)>goal orelse flag  then score(heldList,goal)
	    else
		case moves of
		    [] => current_state(cardList, heldList, moves, true)
		  | x::xs' => case x of
				  Discard card => current_state(cardList,
								remove_card(heldList,card,IllegalMove)
								, xs', false)
				| Draw => if null cardList then current_state(cardList
									     , heldList, moves, true)
					  else current_state(#1 (draw_card(cardList,heldList)),
							     #2 (draw_card(cardList,heldList)),
							     xs', false)
    in
	current_state(cardList,[],moves,false)
    end

