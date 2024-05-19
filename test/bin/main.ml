let rec member c t l = match l with
| [] -> false
| x::xs -> if c x t = 0 then true else member c t xs;;

let rec equal_second_components 