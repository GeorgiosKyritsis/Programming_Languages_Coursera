(* Takes two dates and evaluates to true or false. True when the first date comes before the second date *)
fun is_older(date1: int*int*int, date2: int*int*int)=
	if #1 date1 = #1 date2
	then 
	    if #2 date1 = #2 date2
	    then #3 date1 < #3 date2
	    else #2 date1 < #2 date2
	else
	    #1 date1 < #1 date2

(* Tests *)
val is_older1 = is_older((1970,1,1), (1970,1,2))
val  is_older2 = is_older((1969,1,1), (1970,1,1))


(* Takes a list of dates and a month (i.e., an int) and returns how many dates in the list are in the given month *)
fun number_in_month(dates: (int*int*int) list, month: int)= 
  if null dates then 0
  else
      if #2 (hd dates) = month 
      then 1 + number_in_month(tl dates, month)
      else number_in_month(tl dates, month)

(* Tests *)
val number_in_months_x =  number_in_month([(1970,1,1), (1980,1,5), (1888,1,8)], 1)

(* Using pattern matching *)
fun number_in_month_case(dates, month)=
  case dates of
      [] => 0
   | (_,x,_)::xs => if x = month 
	      then 1 + number_in_month_case(xs, month)
	      else  
		  number_in_month_case(xs, month)

(* Tests *)
val number_in_months_y =  number_in_month_case([(1970,1,1), (1980,1,5), (1888,1,8), (1888,2,5), (1821,1,1)], 1)


(* Takes a list of dates and a list of months (i.e., an int list) and returns the number of dates in the list of dates that are in any of the months in the list of months. Assume the list of months has no number repeated *)
fun number_in_months(dates: (int*int*int) list, months: int list)=
  if null months then 0
  else
      number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* Tests *)
val number_in_months11 = number_in_months([],[]) = 0
val number_in_months2 = number_in_months([], [1,4]) = 0
val number_in_months3 = number_in_months([(1999,1,2), (1240,2,2), (1987,12,1), (2000,12,1)], [1,2]) = 2
val number_in_months4 = number_in_months([(1900,1,1), (1800,1,1), (1999,2,2)], [1,3,4]) = 2


(* using pattern matching *)
fun number_in_months_case(dates, months)=
  case months of
      [] => 0
   | x::xs => number_in_month_case(dates, x) + number_in_months_case(dates, xs)

(* Tests *)
val number_in_months_case1 = number_in_months_case([],[]) = 0
val number_in_months_case2 = number_in_months_case([], [1,4]) = 0
val number_in_months_case3 = number_in_months_case([(1999,1,2), (1240,2,2), (1987,12,1), (2000,12,1)], [1,2]) = 2
val number_in_months_case4 = number_in_months_case([(1900,1,1), (1800,1,1), (1999,2,2)], [1,3,4]) = 2

 
(* Takes a list of dates and a month (i.e., an int) and returns a list holding the dates from the argument list of dates that are in the month. The returned list should contain dates in the order they were originally given *)
fun dates_in_month(dates: (int*int*int) list, month: int)=
  if null dates then []
  else 
      if #2 (hd dates) = month
      then (hd dates) :: dates_in_month(tl dates, month)
      else dates_in_month(tl dates, month)
 
(* Tests *)
val dates_in_month1 = dates_in_month([(1900,1,2), (1980,3,3), (1999,4,4), (1888,1,1)], 1) = [(1900,1,2), (1888,1,1)]
val dates_in_month2 = dates_in_month([(1900,1,1)], 2) = []


(* using pattern matching *)
fun dates_in_month_case(dates, month)= 
  case dates of
      [] => []
   | (x1,x2,x3)::xs => if x2 = month
		    then (x1,x2,x3)::dates_in_month_case(xs,month)
		    else dates_in_month_case(xs,month)

(* Tests *)
val dates_in_month_case1 = dates_in_month([(1900,1,2), (1980,3,3), (1999,4,4), (1888,1,1)], 1) = [(1900,1,2), (1888,1,1)]
val dates_in_month_case2 = dates_in_month([(1900,1,1)], 2) = []


(* Takes a list of dates and a list of months (i.e., an int list) and returns a list holding the dates from the argument list of dates that are in any of the months in the list of months. Assume the list of months has no number repeated. *)
fun dates_in_months(dates: (int*int*int) list, months: int list)=
  if null months then []
  else
      dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* Tests *)
val dates_in_months1 = dates_in_months([], []) = []
val dates_in_months2 = dates_in_months([(1000,1,1), (1200,2,2), (1300,2,2), (1400,4,4),(1600, 5,5)], [1,2,5]) = [(1000,1,1), (1200,2,2), (1300,2,2), (1600,5,5)]


(* using pattern matching *)
fun dates_in_months_cases(dates, months)=
  case months of
      [] => []
   | x::xs => dates_in_month_case(dates, hd months) @ dates_in_months_cases(dates, tl months)

(* Tests *)
val dates_in_months_case1 = dates_in_months([], []) = []
val dates_in_months_case2 = dates_in_months([(1000,1,1), (1200,2,2), (1300,2,2), (1400,4,4),(1600, 5,5)], [1,2,5]) = [(1000,1,1), (1200,2,2), (1300,2,2), (1600,5,5)]


(* Takes a list of strings and an int n and returns the n th element of the list where the head of the list is 1 st *)
fun get_nth(str: string list, n: int)=
  if n=1 then hd str
  else get_nth(tl str, n-1)

(* Tests *)
val get_nth1 = get_nth(["a", "b", "c", "d", "e"], 3) = "c"
val get_nth2 = get_nth(["w","e"], 2) = "e"


(* using pattern matching *)
fun get_nth_case(str, n)=
  case n of
      1 => hd str
   | _ => get_nth(tl str, n-1)

(* Tests *)
val get_nth_case1 = get_nth_case(["a", "b", "c", "d", "e"], 3) = "c"
val get_nth_case2 = get_nth_case(["w","e"], 2) = "e"


(* takes a date and returns a string of the form January 20, 2013 (for example) *)
fun date_to_string(date: int*int*int)=
  let
      val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
      get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

(* Tests *)
val date_to_string1 = date_to_string((1900,12,1))
val date_to_string2 = date_to_string((1987,2,1))
 

(* using pattern matching *)
fun date_to_string_case date =
  case date of
      (x1,1,x3) => "January" ^ " " ^ Int.toString(x3) ^ ", " ^ Int.toString(x1)
   | (x1,2,x3) => "February" ^ " " ^ Int.toString(x3) ^ ", " ^ Int.toString(x1)
   | (x1,3,x3) => "March" ^ " " ^ Int.toString(x3) ^ ", " ^ Int.toString(x1)
   | (x1,4,x3) => "April" ^ " " ^ Int.toString(x3) ^ ", " ^ Int.toString(x1)
   | (x1,5,x3) => "May" ^ " " ^ Int.toString(x3) ^ ", " ^ Int.toString(x1)
   | (x1,6,x3) => "June" ^ " " ^ Int.toString(x3) ^ ", " ^ Int.toString(x1)
   | (x1,7,x3) => "July" ^ " " ^ Int.toString(x3) ^ ", " ^ Int.toString(x1)
   | (x1,8,x3) => "August" ^ " " ^ Int.toString(x3) ^ ", " ^ Int.toString(x1)
   | (x1,9,x3) => "September" ^ " " ^ Int.toString(x3) ^ ", " ^ Int.toString(x1)
   | (x1,10,x3) => "October" ^ " " ^ Int.toString(x3) ^ ", " ^ Int.toString(x1)
   | (x1,11,x3) => "November" ^ " " ^ Int.toString(x3) ^ ", " ^ Int.toString(x1)
   | (x1,12,x3) => "December" ^ " " ^ Int.toString(x3) ^ ", " ^ Int.toString(x1)        

    
(* Tests *)
val date_to_string_case1 = date_to_string_case((1900,12,1))
val date_to_string_case2 = date_to_string_case((1987,2,1))


(* Takes an int called sum, which you can assume
is positive, and an int list, which you can assume contains all positive numbers, and returns an int. You should return an int n such that the first n elements of the list add to less than sum, but the first n + 1 elements of the list add to sum or more *)
fun number_before_reaching_sum(sum: int, l: int list)=
  let
      fun adder(li: int list, acc: int, index:int)=
	if acc <= 0 then index-1
	else 
	    adder(tl li, acc - hd li, index+1)
  in
      adder(l,sum,0)
end

(* Tests *)
val number_before_reaching_sum1 = number_before_reaching_sum(13, [1,2,3,2,4,2,7])

(* fun number_before_reaching_sum_case(sum, l)= *)
(* I could not do this :-( *)


(* Takes a day of year (i.e., an int between 1 and 365) and returns what month that day is in (1 for January, 2 for February, etc.) *)
fun what_month(day: int)=
  let
      val days = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
      number_before_reaching_sum(day, days) + 1
  end
     
(* Tests *)
val what_month1 = what_month(33) = 2
val what_month2 = what_month(1) = 1
val what_month3 = what_month(150) = 5


(* Takes two days of the year day1 and day2 and returns an int list [m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month of day day2 *)
fun month_range(day1: int, day2: int)=
  if day1 > day2 then []
  else what_month(day1)::month_range(day1+1, day2)

(* Tests *)
val month_range1 = month_range(1,3) = [1,1,1]
val month_range2 = month_range(30,33) = [1,1,2,2]


(* Takes a list of dates and evaluates to an (int*int*int) option. It evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list *)
fun oldest(dates: (int*int*int) list)=
  if null dates then NONE
  else 
      let
	  fun max(dates: (int*int*int) list)=
	    if null (tl dates) then hd dates
	    else
		let val maximum_tail = max(tl dates)
		in
		    if is_older(hd dates, maximum_tail)
		    then hd dates
		    else maximum_tail
		end
      in
	 SOME (max dates)
      end

(* Tests *)
val oldest1 = oldest([]) = NONE
val oldest2 = oldest([(1980,1,1), (1900,1,1), (2000,1,1)]) = SOME(1900,1,1)
  

(* using pattern matching *)
fun oldest_case dates =
let
    fun the_oldest dates=
      case dates of
	  x::[] => x
       | x::xs => let val maximum_tail = the_oldest xs
		  in
		      if is_older(x,maximum_tail)
		      then x
		      else maximum_tail
		  end
in
  case dates of
      [] => NONE
   | x::[] => SOME x
   | x::xs => SOME (the_oldest dates)
end

(* Tests *)
val oldest_case1 = oldest_case([]) = NONE
val oldest_case2 = oldest_case([(1980,1,1), (1900,1,1), (2000,1,1)]) = SOME(1900,1,1)


(* Helper function. Remove an integer from an integer list *)
fun delete(num: int, l: int list)=
  if null l then []
  else 
      if hd l = num then delete(num, tl l)
      else hd l :: delete(num, tl l)

(* Helper function. Remove duplicates from list *)
fun removeDuplicates(l: int list)=
  if null l then []
  else hd l :: removeDuplicates(delete(hd l, tl l))


(* Takes a list of dates and a list of months (i.e., an int list) and returns the number of dates in the list of dates that are in any of the months in the list of months. We eliminate the duplicates from the months list *)
fun number_in_months_challenge(dates: (int*int*int) list, months: int list)=
  number_in_months(dates, removeDuplicates(months))

(* Tests *)
val number_in_months_challenge1 = number_in_months_challenge([(1900,1,1), (1987,2,2), (1999,1,1)], [1,2,2,3]) = 3


(* Helper function. Using pattern matching *)
fun delete_case(num, l)= 
  case l of
      [] => []
   | x::xs => if x=num then delete(num,xs)
	      else x::delete(num,xs)

(* Helper Function. Using pattern matching *)
fun removeDuplicates_case l = 
  case l of
      [] => []
   | x::xs => x::removeDuplicates_case(delete_case(x, xs))


(* using pattern matching *)
fun number_in_months_challenge_case(dates, months)=
  number_in_months_case(dates, removeDuplicates_case months)
  
(* Tests *)
val number_in_months_challenge_case1 = number_in_months_challenge_case([(1900,1,1), (1987,2,2), (1999,1,1)], [1,2,2,3]) = 3
 

(* Takes a list of dates and a list of months (i.e., an int list) and returns a list holding the dates from the argument list of dates that are in any of the months in the list of months. Assume the list of months has no number repeated. We eliminate the duplicates from the months list *)
fun dates_in_months_challenge(dates: (int*int*int) list, months: int list)=
  dates_in_months(dates, removeDuplicates months)

(* Tests *)
val dates_in_months_challenge1 = dates_in_months_challenge([], []) = []
val dates_in_months_challenge2 = dates_in_months_challenge([(1000,1,1), (1200,2,2), (1300,2,2), (1400,4,4),(1600, 5,5)], [1,2,5]) = [(1000,1,1), (1200,2,2), (1300,2,2), (1600,5,5)]

fun dates_in_months_challenge_case(dates, months)=
  dates_in_months_cases(dates, removeDuplicates_case months)

(* Tests *)
val dates_in_months_challenge_case1 = dates_in_months_challenge_case([], []) = []
val dates_in_months_challenge_case2 = dates_in_months_challenge_case([(1000,1,1), (1200,2,2), (1300,2,2), (1400,4,4),(1600, 5,5)], [1,2,5]) = [(1000,1,1), (1200,2,2), (1300,2,2), (1600,5,5)]


(* Takes a date and determines if it describes a real date in the common era *)
fun reasonable_date(date: int*int*int)=
  let
      fun is_valid_year(year: int)= year > 0

      fun is_valid_month(month: int)= 1<=month andalso month<=12

      val days_nonleap = [31,28,31,30,31,30,31,31,30,31,30,31]

      val days_leap = [31,29,31,30,31,30,31,31,30,31,30,31]

      fun is_leap(year: int)= 
	(year mod 400)=0 orelse (year mod 4)=0 andalso (year mod 100) <> 0 

      fun get_days(days: int list, n: int)=
	if n=1 then hd days
	else get_days(tl days, n-1)

      fun is_valid_day(year: int,month: int)=
	if is_leap(year)
	then #3 date >=0 andalso #3 date <= get_days(days_leap, month)
	else #3 date >= 0 andalso #3 date <= get_days(days_nonleap, month)

  in
      is_valid_year(#1 date) andalso is_valid_month(#2 date) andalso is_valid_day(#1 date, #2 date)
  end

(* Tests *)
val reasonable_date1 = reasonable_date((1996,2,29)) = true (* leap year *)
val reasonable_date2 = reasonable_date((1980,4,31)) = false
val reasonable_date3 = reasonable_date((1900,1,1)) = true
	    
	



