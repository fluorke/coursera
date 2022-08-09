fun alternate (xs : int list) =
    if null xs
    then 0
    else hd xs - hd (tl xs) + alternate(tl (tl xs))

fun min (xs : int list) =
    if null (tl xs)
    then hd xs
    else
	let
	    val a = hd xs		       
	    val b = min(tl xs)
	in if a < b
	   then a
	   else b
	end

fun max (xs : int list) =
    if null (tl xs)
    then hd xs
    else
	let
	    val a = hd xs		       
	    val b = min(tl xs)
	in if a > b
	   then a
	   else b
	end
		   
fun min_max (xs : int list) =
    let
	val minimal = min(xs)
	val maximum = max(xs)
    in (minimal, maximum)
    end
 		    
fun sum (xs : int list) =
    if null xs
    then 0
    else hd xs + sum(tl xs)

fun sum_wrapper (xs : int list) =
    sum(xs) :: []

fun cumsum (xs : int list) =
    if null xs
    then []
    else cumsum(tl xs) @ sum_wrapper(revers(xs))

fun revers (xs : int list) =
    if null xs
    then []
    else revers(tl xs) @ [hd xs]

fun greeting (name: string option) =
    if name = NONE then "Hello there, you!"
    else "Hello there, " ^ valOf name ^ "!"
					   
fun repeat (xs : int list, cs: int list) =
    if null xs andalso null cs
    then []
    else
	fill(hd xs, hd cs) @ repeat (tl xs, tl cs)


fun fill (x : int, count : int) =
    if count = 0
    then []
    else x :: fill(x, count - 1)
