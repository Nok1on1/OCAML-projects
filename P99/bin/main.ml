
let pack lst =
  let rec pack lst lst1 lst2 x =
    match lst with
    | [] -> lst1
    | h :: t -> if h = x then pack t lst1 (h::lst2) h else  pack t ((x::lst2)::lst1) [] h
  in pack lst [] [] " ";;


let runlength lst = 
  let rec runlength1 lst lst1 var count =
    match lst with
    | [] -> lst1 @ [(var, (count+1))]
    | h::t -> if h = var then runlength1 t lst1 var (count+1) else runlength1 t (lst1@ [(var,(count+1))]) h 0 
  in
  let first = function
  | [] -> raise Not_found
  | h::_t -> h
in 
  runlength1 lst [] (first lst) 0;;

type 'a node = 
  | One of 'a
  | Many of 'a node list;;

let flatten lst = 
  let rec flatten1 lst lst2 =
    match lst with
    | [] -> lst2
    | One h::t -> flatten1 t (lst2 @ [h])    
    | Many h::t -> flatten1 t (flatten1 h lst2)
  in flatten1 lst [];;