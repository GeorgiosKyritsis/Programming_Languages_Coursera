fun is_older(date1: int*int*int, date2: int*int*int)=
  if (#1 date1) = (#1 date2)
  then if (#2 date1) = (#2 date2) then (#3 date1) < (#3 date2)
       else (#2 date1) < (#2 date2)
  else 
      (#1 date1) < (#1 date2)

(* Some Tests for the is_older function *)

val is_older1 =  is_older((1987,2,1), (1987,2,1)) = false
val is_older2 = is_older((1987,3,15), (1987,3,16)) = true
val is_older3 = is_older((1987,3,15), (1987,3,14)) = false
val is_older4 = is_older((1987,3,15), (1987,2,15)) = false
val is_older5 = is_older((1987,3,15), (1987,4,15)) = true
val is_older6 = is_older((1987,3,15), (1985,3,15)) = false
val is_older7 = is_older((1987,3,15), (1988,3,15)) = true

fun number_in_month(dates: (int*int*int) list, month: int)=
  if null dates then 0
  else 
      if #2 (hd dates) = month then 1 + number_in_month(tl dates, month)
      else number_in_month(tl dates, month)

(* Some tests for the number_in_month *)
val number_in_month1 = number_in_month([(1987,2,1), (1988,2,2), (1988,3,3)], 2) = 2
val number_in_month2 = number_in_month([(1988,2,1)], 1) = 0
val number_in_month3 = number_in_month([], 1) = 0

fun number_in_months(dates: (int*int*int) list, months: int list)=
  if null months then 0
  else number_in_month(dates, hd months) + number_in_months(dates,tl months)

(* Some tests for the number_in_months *)
val number_in_months1 = number_in_months([],[]) = 0
val number_in_months2 = number_in_months([], [1,4]) = 0
val number_in_months3 = number_in_months([(1999,1,2), (1240,2,2), (1987,12,1), (2000,12,1)], [1,2]) = 2
val number_in_months4 = number_in_months([(1900,1,1), (1800,1,1), (1999,2,2)], [1,3,4]) = 2

fun dates_in_month(dates: (int*int*int) list, month: int)=
  if null dates
  then []
  else 
      if #2 (hd dates) = month then hd dates :: dates_in_month(tl dates, month)
      else dates_in_month(tl dates, month)

(* Some tests for the dates_in_month *)
val dates_in_month1 = dates_in_month([(1900,1,2), (1980,3,3), (1999,4,4), (1888,1,1)], 1) = [(1900,1,2), (1888,1,1)]
val dates_in_month2 = dates_in_month([(1900,1,1)], 2) = []

fun dates_in_months(dates: (int*int*int) list, months: int list)=
  if null months then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* Some tests for the dates_in_months *)
val dates_in_months1 = dates_in_months([], []) = []
val dates_in_months2 = dates_in_months([(1000,1,1), (1200,2,2), (1300,2,2), (1400,4,4),(1600, 5,5)], [1,2,5]) = [(1000,1,1), (1200,2,2), (1300,2,2), (1600,5,5)]


fun get_nth(strings: string list, n: int)=
  if n = 1 then hd strings
  else get_nth(tl strings, n-1)

(* Some tests for the get_nth *)
val get_nth1 = get_nth(["a", "b", "c", "d", "e"], 3) = "c"
val get_nth2 = get_nth(["w","e"], 2) = "e"

fun date_to_string(date: int*int*int)=
  let
      val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
      get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

(* Some tests for the date_to_string *)
val date_to_string1 = date_to_string((1900,12,1))
val date_to_string2 = date_to_string((1987,2,1))


fun number_before_reaching_sum(sum: int, numbers : int list)= 
  if sum - hd numbers > 0
  then 1  + number_before_reaching_sum(sum - hd numbers, tl numbers)
  else 0

(* Some tests for the number_before_reaching_the_sum *)
val number_before_reaching_sum1 = number_before_reaching_sum(13, [1,2,3,4,5,6,7]) = 4

fun what_month(day: int)=
  let
      val month = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
      number_before_reaching_sum(day, month) + 1
  end

(* Some tests for the what_month *)
val what_month1 = what_month(33) = 2
val what_month2 = what_month(1) = 1
val what_month3 = what_month(150) = 5


fun month_range(day1: int, day2: int)=
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1+1,day2)

(* Some tests for the month_range *)
val month_range1 = month_range(1,3) = [1,1,1]
val month_range2 = month_range(30,33) = [1,1,2,2]

fun oldest(dates: (int*int*int) list)=
  if null dates
  then NONE
  else let
      fun the_oldest(dates: (int*int*int) list)=
	if null (tl dates)
	then hd dates
	else
	    let val tl_ans = the_oldest(tl dates)
	    in
		if is_older(hd dates, tl_ans)
		then hd dates
		else tl_ans
	    end
  in
      SOME (the_oldest dates)
  end

(* Some tests for the oldest *)
val oldest1 = oldest([]) = NONE
val oldest2 = oldest([(1980,1,1), (1900,1,1), (2000,1,1)]) = SOME(1900,1,1)

(* Helper function *)
fun delete(e: int, l: int list)=
  if null l
  then []
  else 
      if e = hd l then delete(e, tl l)
      else hd l :: delete(e, tl l)

(* Helper function *)
fun removeDuplicates(months: int list)=
  if null months
  then []
  else hd months :: removeDuplicates(delete(hd months, tl months))


fun number_in_months_challenge(dates: (int*int*int) list, months: int list)=
  number_in_months(dates, removeDuplicates(months))

(* Some tests for the the oldest *)
val number_in_months_challeng1 = number_in_months_challenge([(1900,1,1), (1987,2,2), (1999,1,1)], [1,2,2,3]) = 3

fun reasonable_date(date: (int*int*int))=
  let
      val days_in_month = [31,28,31,30,31,30,31,31,30,31,30,31]
      val days_in_month_leap = [31,29,31,30,31,30,31,31,30,31,30,31]

      fun is_leap(year: int)=
	year mod 400 = 0 orelse year mod 4 = 0 andalso year mod 100 <> 0

      fun get_pos(ints: int list, n: int)=
	if n = 1 
	then hd ints
	else get_pos(tl ints, n-1)

      fun is_valid_year(year: int)= year > 0
      fun is_valid_month(month: int)= month >=1 andalso month <= 12
      fun is_valid_day(day: int)= day >= 1 andalso day <= 31

      fun is_valid_day_leap()=
	if is_leap(#1 date)
	then (#3 date) <= get_pos(days_in_month_leap, #2 date)
	else (#3 date) <= get_pos(days_in_month, #2 date)
  in
      is_valid_year(#1 date) andalso is_valid_month(#2 date) andalso is_valid_day(#3 date) andalso is_valid_day_leap()
  end

(* Some tests for reasonable_date *)
val reasonable_date1 = reasonable_date((1996,2,29)) = true (* leap year *)
val reasonable_date2 = reasonable_date((1980,4,31)) = false
val reasonable_date3 = reasonable_date((1900,1,1)) = true



