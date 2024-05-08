
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


(*find the smallest*)
let rec mklst string =
  match String.fold_left (fun (x, ind) y -> (x @ [((int_of_char y - 48), ind)], (ind + 1))) ([], 0) string with
  | (a, _) -> a

let rec toint ((num, move) : string * (int * int)) : int * (int * int) =
  if num.[0] = '0' then int_of_string (String.sub num 1 (String.length num - 1)), move
  else int_of_string num, move

let put (lst : (int * int) list) ((n, inde) : (int * int)) (index : int) : (string * (int * int)) =
  let ogindex = index in 
  let rec aux lst (n, inde) index str  =
    match lst, index with
    | [], _ -> ((if index = 0 then str ^ string_of_int n else str), (inde, ogindex))
    | (x, ind)::xs , 0 -> 
        if (x, ind) = (n, inde) then
          aux xs (n, inde) (index-1) (str ^ string_of_int x)
        else
          aux xs (n , inde) (index-1) (str ^ string_of_int n ^ string_of_int x)
    | (x, ind)::xs , _ -> 
        if (x, ind) = (n, inde) then
          aux xs (n, inde) index str
        else
          aux xs (n, inde) (index-1) (str ^ string_of_int x)
  in aux lst ((n, inde) : (int * int)) (index : int) ""

let rec call (lst : (int * int) list) ((n, inde) : (int * int)) (range : int) =
  match range with
  | 0 -> [toint (put lst (n, inde) 0)]
  | _ -> (toint (put lst (n, inde) range)) :: call lst (n, inde) (range-1)

let allpossible (string : string) =
  let oglst = mklst string in
  let oglstlen = List.length oglst in
  let rec assembly lst (alllist : (int * (int*int)) list) : (int*(int*int)) list =
    match lst with
    | [] -> alllist
    | (x, ind)::xs -> assembly xs ((call oglst (x, ind) (oglstlen-1))) @ alllist
  in assembly oglst []

let rec bestsuit (lst : (int * (int*int)) list) (best, (og, nog)) =
  match lst with
  | [] -> (best, (og, nog))
  | (num, (og1, nog1))::xs ->
      if num < best then bestsuit xs (num, (og1, nog1))
      else if num = best then
        if og < og1 then bestsuit xs (best, (og, nog))
        else bestsuit xs (num, (og1, nog1))
      else bestsuit xs (best, (og, nog))

let smallest (sn : string): string =
  match bestsuit (allpossible sn) (List.hd (allpossible sn)) with
  | (num, (og, nog)) -> string_of_int num ^ " " ^ string_of_int og ^ " " ^ string_of_int nog
