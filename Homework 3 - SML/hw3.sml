exception NoAnswer

(*Problem 1*)
fun only_capitals(strList)= List.filter (fn s => Char.isUpper(String.sub(s,0))) strList
					
(*Problem 2*)
fun longest_string1(strList)= List.foldl (fn (s1,s2) => if String.size(s1)>String.size(s2)
						       then s1
						       else s2) "" strList
(*Problem 3*)
fun longest_string2(strList)= List.foldl (fn (s1,s2) => if String.size(s1)>=String.size(s2)
							then s1
							else s2) "" strList


(*Problem 4*)
fun longest_string_helper f  =  List.foldl ( fn(s1,s2) => if f (String.size s1,String.size s2)
							  then s1
							  else s2 ) ""
		 
	       
val longest_string3 = longest_string_helper (fn(s1,s2) => s1>s2)
val longest_string4 = longest_string_helper (fn (s1,s2) => s1>=s2)

(*Problem 5*)
val longest_capitalized = longest_string1 o only_capitals

(*Problem 6*)
val rev_string = String.implode o List.rev o String.explode

(*Problem 7*)
fun first_answer f inList=
    case inList of
	[] => raise NoAnswer
      | x::xs' => case f x of
		      SOME v => v
		    | NONE  => first_answer f xs'
					     
(*Problem 8*)
fun all_answers f xs =
    let fun loop (acc,xs) =
        case xs of
		        [] => SOME acc
	        | x::xs' => case f x of 
                          NONE => NONE
              			    | SOME y => loop((y @ acc), xs')
    in loop ([],xs) end

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
	
(*Problem 9*)
val count_wildcards = g (fn _ => 1)(fn _ => 0)
val count_wild_and_variable_lengths =  g (fn _ => 1) String.size
fun count_some_var (string, p)=
    g (fn _ => 0) (fn s => if String.isSubstring string s then 1 else 0) p

(*Problem 10*)
fun check_pat (p)=
    let
	fun extractValues p=
	    case p of
		Variable x        => [x] 
	      | TupleP ps         => List.foldl (fn (p,i) => (extractValues p) @ i) [] ps
	      | ConstructorP(_,p) => extractValues p
	      | _                 => []

	val valList = extractValues p
	fun checkUniqueness(valList) =
	    case valList of
		[] => true
	      | x::xs' => if List.exists (fn e => e=x) xs' then false else checkUniqueness xs'
    in
	checkUniqueness(valList)
    end

(*Problem 11*)
fun match (valu,pat) =
    case (valu,pat) of
	      (_,Wildcard)    => SOME []
      | (_,Variable(s)) => SOME [(s,valu)]
      | (Unit,UnitP)    => SOME []
      | (Const i, ConstP j)    => if i=j then SOME [] else NONE
      | (Tuple(vs),TupleP(ps)) => if length vs = length ps
				  then all_answers match (ListPair.zip(vs,ps))
				  else NONE
      | (Constructor(s1,v), ConstructorP(s2,p)) => if s1=s2
						   then match(v,p)
                                                   else NONE
      | _ => NONE

				 
(*Problem 12*)
fun first_match v plst =
    SOME (first_answer (fn p => match (v, p)) plst)
    handle NoAnswer => NONE				 
