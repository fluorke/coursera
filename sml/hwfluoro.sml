fun is_older (ls : (int * int * int), rs : (int * int * int)) =
    if #1 ls < #1 rs orelse #2 ls < #2 rs orelse #3 ls < #3 rs
    then true
    else false
	     
fun number_in_month (xs : (int * int * int) list, m : int) =
    if null xs
    then 0
    else
	if #2 (hd xs) = m
	then 1 + number_in_month(tl xs, m)
	else number_in_month(tl xs, m)  

fun number_in_months (xs : (int * int * int) list, m : int list) =
    if null m
    then 0
    else
	let val h = number_in_month(xs, (hd m))
	in h + number_in_months(xs, (tl m))
	end

fun dates_in_month (xs : (int * int * int) list, m : int) =	    
    if null xs
    then []
    else
	if #2 (hd xs) = m
	then hd xs :: dates_in_month(tl xs, m)
	else dates_in_month(tl xs, m)

fun dates_in_month (xs : (int * int * int) list, m : int) =	    
    if null xs
    then []
    else
	if #2 (hd xs) = m
	then hd xs :: dates_in_month(tl xs, m)
	else dates_in_month(tl xs, m)

fun dates_in_months (xs : (int * int * int) list, m : int list) =	    
    if null m
    then []
    else
	let val month = hd m
	in dates_in_month(xs, month) @ dates_in_months(xs, tl m)
	end					      

fun get_nth (xs : string list, n : int) =
    if n = 1
    then hd xs
    else get_nth(tl xs, n - 1)
	
val MONTHS = ["January", "February", "March", "April",
"May", "June", "July", "August", "September", "October", "November", "December"]		

fun date_to_string (y : int, m : int, d : int) =
   get_nth(MONTHS, m) ^ " " ^ Int.toString(d) ^ ", " ^ Int.toString(y)
	     
(* [ ] add comments to code *)	     
	 			
fun number_before_reaching_sum (limit : int, xs: int list) =
    if null xs <> true andalso limit - hd xs > 0
    then 1 + number_before_reaching_sum(limit - hd xs, tl xs)
    else 0

val MONTH2 = [0, 31, 60, 91, 122, 152, 183, 214, 244, 275, 334, 365]	     

fun number_before_reaching_month (limit : int, xs: int list) =
    if null xs <> true andalso limit - hd xs > 0
    then 1 + number_before_reaching_month(limit, tl xs)
    else 0
		 
fun what_month (m: int) =
    number_before_reaching_month
	(m, MONTH2)
			       
fun month_range (d1: int, d2: int) =
    if d1 <= d2
    then what_month(d1) :: month_range(d1 + 1, d2)
    else []

(* add NONE exeption for missing or wrong arguments *)	     
fun is_older_month (ls : (int * int * int), rs : (int * int * int)) =
    if #1 ls < #1 rs
    then SOME ls
    else
	if #1 ls = #1 rs andalso #2 ls < #2 rs
	then SOME ls
	else
	    if #2 ls = #2 rs andalso #3 ls <= #3 rs
	    then SOME ls
	    else SOME rs    
	     
fun oldest(xs : (int * int * int) list)	=
    if null xs
    then NONE
    else
	let val r = oldest(tl xs)
	in if isSome r 
        then is_older_month(hd xs, valOf r)
        else SOME (hd xs)			    
	end	  

fun months_has_month (xs : (int * int * int) list, m : int) =
    if null xs
    then false
    else
	if #2 (hd xs) = m
	then true
	else months_has_month(tl xs, m)

fun duplicate_helper (ms : int list, m : int) =
    if null ms
    then 0
    else
	let
	in if hd ms = m
	   then 1 + duplicate_helper(tl ms, m)
	   else duplicate_helper(tl ms, m)		    
	end	    
		    
	    
fun number_in_months_challenge (xs : (int * int * int) list, m : int list) =
    if null m
    then 0
    else
	let
	    val dup = duplicate_helper(m, hd m)
	    val number = if dup > 1 then 0 else number_in_month(xs, hd m)		      
	in number + number_in_months_challenge(xs, tl m)
	end
				      
