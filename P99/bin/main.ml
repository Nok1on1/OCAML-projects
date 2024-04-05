
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

 type 'a rle =
  | One of 'a
  | Many of int * 'a;;
  let runlength lst = 
    let rec runlength1 lst lst1 var count =
      match lst with
      | [] -> lst1 @ (if count = 0 then [One var] else [Many ( (count+1),var)])
      | h::t -> if h = var then runlength1 t lst1 var (count+1)
      else  runlength1 t (if count = 0 then lst1@ [One var] else [Many ( (count+1),var)]) h 0
    in
    let first = function
    | [] -> raise Not_found
    | h::_t -> h
  in 
    runlength1 lst [] (first lst) (-1);;

  let replicate lst n =
    let rec duplicate var x =
      match x with
      | 1 -> var
      | _ -> var ^ duplicate var (x-1) 
      in
    let rec dupN lst lst1 n = 
      match lst with
      | [] -> lst1
      | h::t -> dupN t (lst1 @ [duplicate h n]) n 
    in
    dupN lst [] n;;

  let rotate lst n=
  let rec size n = function
  | [] -> n
  | _h::t -> size (n+1) t 
  in 
    let rec rotate1  lst n =
    match lst, n with
    | [], _ -> []
    | _ , 0 -> lst
    | h::t, _ -> rotate1 (t @ [h]) (n-1)
  in
  rotate1 lst ((size 0 lst) - abs(n));;
