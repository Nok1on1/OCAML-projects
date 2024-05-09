let isuppercase (n : char) = if ((64 < int_of_char n) && (int_of_char n < 91)) then true else false ;;

let first_non_repeating_letter s = 
  let oglst = (s |> String.to_seq |> List.of_seq) in
let rec aux lst (lst2 : char list) (n : char) = match lst with
| [] ->  if lst2 = [] then None else if List.length lst2 = 1 then (Some (List.hd lst2)) else aux (List.tl lst2) [] (List.hd lst2)
| x::xs -> if Char.uppercase_ascii x = n || Char.lowercase_ascii x = n then aux xs (lst2) (n) else aux xs (x::lst2) n
in (if oglst = [] then None else aux (oglst) ([]) (List.hd oglst));;