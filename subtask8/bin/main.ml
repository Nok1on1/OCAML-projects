let rec member c t = function
| x::xs -> if c t x  = 0 then true else member c t xs
| [] -> false;;

let rec occurin x = function
| (y,num)::xs -> if x = y then (y,num) else occurin x xs
| [] -> (x, 0);;

let rec remove x = function
| y::ys -> if y = x then ys else y::remove x ys
| [] -> [];;

let count_occurrences lst = 
  let rec aux lst newlst = match lst with
  | x::xs -> aux xs (match (occurin x newlst) with | (x, num) -> (x,num+1):: (remove (occurin x newlst) newlst))
  | [] -> newlst in aux lst [];;

let rec droplast = function
| [_] -> []
| x::xs -> x::droplast xs
| [] ->  failwith "Empty list has no last
element";;


let rec droplast_opt newlst = function
| [_] -> Some newlst
| x::xs -> droplast_opt (x::newlst) xs
| [] ->  None;;


let rec zip_with f lst1 lst2 = 
  match lst1, lst2 with
  | [], [] -> []
  | x::xs, y::ys -> (f x y)::zip_with f xs ys
  | _, [] -> []
  | [], _ -> [];;

let rec getfirst = function
| (x,_)::xs -> x::getfirst xs
| [] -> [];;
let rec getsecond = function
| (_,x)::xs -> x::getsecond xs
| [] -> [];;

let unzip = function
| (x,y)::xs -> (x::getfirst xs), (y::getsecond xs)
| [] -> ([],[]);;