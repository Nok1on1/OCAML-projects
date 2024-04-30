let index a lst =
let rec aux a lst index =
  match lst with
  | [] -> raise Not_found
  | x::xs -> if x = a then index else aux a xs (index+1)
in aux a lst 0 ;;

let rec ix lst a x j k = match lst with
| [] -> 1
| z::zs -> if j = k then ix zs a  

let lx lst a =
  let reallist = lst in 
let rec aux lst a j =
  match lst with
  | [] -> []
  | (x, y)::tail -> y * (ix reallist a x j 0) * aux tail a (j+1)
in aux lst a 0;;


