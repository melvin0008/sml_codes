(*1*)
fun is_older(dt1 : int * int * int ,dt2: int *int * int)=
    
		if ((#1 dt1) > (#1 dt2))
                then false
                else if((#1 dt1) < (#1 dt2))
                     then true
                     else if (#1 dt1 = #1 dt2)
                          then if(#2 dt1 ) > (#2 dt2)
                               then false
                               else if (#2 dt1 ) < (#2 dt2)
                                    then true
                                    else if (#2 dt1 = #2 dt2)  
			                 then if(#3 dt1 ) < (#3 dt2)
                                              then true 
                                              else false
                                         else false
                           else false
	   
    

(*2*)
fun number_in_month(dtlist: (int * int * int) list ,mo: int)=
    let
       fun calcul (dt : (int * int * int) )=
	   if (mo = #2dt)
	   then 1
	   else 0
    in
	if null dtlist 
	then 0
	else if null (tl dtlist)
             then calcul(hd dtlist)
	      else calcul (hd dtlist)+number_in_month(tl dtlist,mo)
    end
(*3*)     
fun number_in_months(dtlists : (int * int * int ) list ,monthlist : (int) list)=
    if null monthlist 
	then 0
	else if null (tl monthlist)
             then number_in_month(dtlists,hd monthlist)
	      else number_in_month(dtlists,hd monthlist)+number_in_months(dtlists,tl monthlist)

(*4*)
fun dates_in_month(dtlist: (int * int * int) list ,mo: int)=
    let
       fun calcul (dt : (int * int * int) )=
	   if (mo = #2dt)
	   then [dt]
	   else []
    in
	if null dtlist 
	then []
	else if null (tl dtlist)
             then calcul(hd dtlist)
	      else calcul (hd dtlist)@dates_in_month(tl dtlist,mo)
    end
     
(*5*)
fun dates_in_months(dtlist: (int * int * int) list ,monthlists:(int) list)=
       	if null monthlists 
	then []
	else if null (tl monthlists)
             then dates_in_month(dtlist,hd monthlists)
	      else dates_in_month (dtlist,hd monthlists)@dates_in_months(dtlist,tl monthlists)
(*6*)    
fun get_nth(strlist:string list,cnt:int)=
    let
	fun chk(l:int)=
	    l=1
    in 
	if chk(cnt)
	then if null strlist
              then ""
              else hd strlist
        else if null( tl strlist)
             then ""
             else get_nth(tl strlist,cnt-1)
             
     end
(*7*)
fun date_to_string((dt:int * int * int))=
    let
	val monthlist=["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(monthlist,#2dt)^" "^Int.toString(#3dt)^","^" "^Int.toString(#1dt)
    end

(*8*)
fun number_before_reaching_sum(tot:int ,number_list: int list)=
    let
        fun sum1(i: int, n: int, number_list: int list) =
            if null number_list
            then n
            else
                if i + (hd number_list) >= tot
                then n
                else sum1(i + (hd number_list), n+1 , (tl number_list))
    in
        sum1(0, 0, number_list)
    end
(*9*)
fun what_month(day:int)=
     let
	 val monthlist=[31,28,31,30,31,30,31,31,30,31,30,31]
     in
	1+ number_before_reaching_sum(day,monthlist)
     end
(*10*)
fun month_range(day1:int,day2:int)=
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1+1,day2)


(*fun greater(list: (int) list)=
    if null list
    then NONE
    else let
	fun gt(lt:(int) list,i :int ,j : int)=
	    if null ( tl lt)
	    then if i > j
	         then if i > (hd lt)
			then  i
			else  hd lt
		else if j > hd lt
		     then  j
	             else  hd lt
	    else if i > j
                 then gt(tl lt,i,hd lt)
                 else gt(tl lt, hd lt,j)
         in 

	     SOME (gt(list,0,hd list ))
          end
*)

(*11*)
fun oldest(dtlist: (int *int *int )list)=
    if null dtlist
    then NONE
    else let
	fun older(lt:(int * int *int) list,i :(int * int * int) ,j : (int * int * int))=
	    if null ( tl lt)
	    then if is_older(i,j)
	         then if is_older(i , (hd lt))
			then  i
			else  hd lt
		else if is_older(j ,( hd lt))
		     then  j
	             else  hd lt
	    else if is_older(i , j)
                 then older(tl lt,i,hd lt)
                 else older(tl lt, hd lt,j)
         in 

	     SOME (older(dtlist, hd dtlist,hd dtlist ))
          end

(*12*)
fun rem(mo: int list)=
    let 
           fun find(key:int,hay:int list)=
	   if null hay
           then false
           else if key = (hd hay)
                then true
                else find(key,tl hay )
      in
             if null mo
             then []
             else
		 if find(hd mo,tl mo)
		 then rem(tl mo)
		 else hd mo ::rem(tl mo)   
    end

fun number_in_months_challenge(dts: (int*int*int) list, months: int list) =
    number_in_months(dts, rem(months))

fun dates_in_months_challenge(dts: (int*int*int) list, months: int list) =
    dates_in_months(dts, rem(months))

(*13*)
fun reasonable_date(dt:(int * int * int))=
	  let
                fun leap(yr:int)=
		    if (yr mod 4) <> 0
                    then false
	            else if (yr mod 100)<>0
                         then true
		         else if (yr mod 400)<>0
                              then false
                              else true
		fun noofdays(mo:int)=
		    if mo = 2
                    then if leap(#1dt)
		         then 29
                          else 28
	            else if mo <8
                          then if mo mod 2 = 1
			       then 31
                               else 30
                          else if mo mod 2 =0
                                then 31
                                else 30
            in 
                    
		if(#1dt>0)
		then if (#2dt)>0 andalso (#2dt<=12)
		     then if(#3dt)<=(noofdays(#2dt)) andalso (#3dt>0)
			  then true
			  else false
                     else false
		else false
	     end

      
