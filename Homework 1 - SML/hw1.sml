(* Problem 1 *)
fun is_older(date1 : int*int*int, date2 : int*int*int)=
    (* Compare years *)
    if #1 date1 < #1 date2 then true
    else if #1 date1 > #1 date2 then false
    (* Compare months *)
    else
	if #2 date1 < #2 date2 then true
	else if #2 date1 > #2 date2 then false
	(* Compare days *)				     
	else
	    if #3 date1 < #3 date2 then true
	    else false

(* Problem 2 *)
fun number_in_month(dateList : (int*int*int) list, month : int)=
    let
	(* Counter to count how many months have appeared in the input *)
	fun counter(dateList : (int*int*int) list, c : int)=
	    if null dateList then c
	    else if #2 (hd dateList) = month then counter(tl dateList, c+1)
	    else counter(tl dateList, c)	    
    in
	counter(dateList, 0)
    end

(* Problem 3 *)
fun number_in_months(dateList : (int*int*int) list, monthList : int list)=
    let
	(* Check if the input month exists in the input list*)
	fun monthCompare(month : int, monthList : int list)=
	    if null monthList then false
	    else if  hd monthList = month then true
	    else monthCompare(month, tl monthList)

	(* Counter to count how many months have appeared in the input *)
	fun counter(dateList : (int*int*int) list, c : int)=
	    if null dateList then c
	    else if monthCompare(#2 (hd dateList), monthList)then counter(tl dateList, c+1)
	    else counter(tl dateList, c)	    
    in
	counter(dateList, 0)
    end

(* Problem 4*)
fun dates_in_month(dateList : (int*int*int) list, month : int)=
    if null dateList then []
    else if #2 (hd dateList) = month then (hd dateList)::dates_in_month(tl dateList, month)
    else dates_in_month(tl dateList, month)


(* Problem 5*)
fun dates_in_months(dateList : (int*int*int) list, monthList : int list)=
    let
	(* Check if the input month exists in the input list*)
	fun monthCompare(month : int, monthList : int list)=
	    if null monthList then false
	    else if  hd monthList = month then true
	    else monthCompare(month, tl monthList)
    in
	    if null dateList then []
	    else if monthCompare(#2 (hd dateList), monthList)
	    then (hd dateList)::dates_in_months(tl dateList, monthList)
	    else dates_in_months(tl dateList, monthList)
    end
	
(*Problem 6*)
fun get_nth(strList : string list, index : int)=
    if index = 1 then hd strList
    else get_nth(tl strList, index-1)


(*Problem 7*)
fun date_to_string(date : (int*int*int))=
    let
	val months = ["January", "February", "March", "April"," May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(months, #2 date )^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)
    end

(*Problem 8*)
fun number_before_reaching_sum (sum : int, numList : int list)=
    let
	fun check(stack : int, index : int, numList : int list)=
	    if (stack + hd numList) >=  sum then index
	    else check (stack + hd numList, index + 1, tl  numList)
    in
	check(0, 0, numList)
    end

(*Problem 9*)
fun what_month(day : int)=
    let
	val months = [(1,0,31),(2,31,59),(3,59,90),(4,90,120),(5,120,151),
		      (6,151,181),(7,181,212),(8,212,243),(9,243,273),(10,273,304)
		      ,(11,304,334),(12,334,365)]
	fun check(months : (int*int*int) list)=
	    if day > #2 (hd months) andalso day <= #3 (hd months)
	    then #1 (hd months)
	    else check (tl months)
    in
	check(months)
    end

			  
(*Problem 10*)
fun month_range(day1 : int, day2 : int)=
    if day1 > day2 then []
    else if day1 = day2 then [what_month(day2)]
    else what_month(day1)::month_range(day1+1,day2)

(*Problem 11*)
fun oldest(dates : (int*int*int) list)=
    let
	fun check(date : (int*int*int), dates : (int*int*int) list)=
	    if null dates then SOME date
	    else if is_older(date, hd dates) then check (date, tl dates)
	    else check (hd dates, tl dates)
    in
	if dates = [] then NONE
	else check(hd dates, tl dates)
    end
	
    

