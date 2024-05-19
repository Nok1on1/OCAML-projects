
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


(*weight for weight*)
let cut str = String.split_on_char ' ' str;;


let weight str = 
  let rec aux = function
  | [] -> 0
  | x::xs -> (int_of_char x)-48+ (aux xs)
in aux (String.to_seq str |> List.of_seq) ;;

let rec tuples = function
| [] -> []
| x::xs -> (x, weight x)::(tuples xs);;

let alphabetically a b =
let rec aux a b = match a , b with
| [], [] -> 0
| x::xs, y::ys -> if x > y then 1 else if x < y then -1 else aux xs ys 
| [], _ -> -1
| _ , [] -> 1
in aux (String.to_seq a |> List.of_seq) (String.to_seq b |> List.of_seq);;

let order tuplelst = List.sort (fun (x, weight) (y, weight1) -> if weight-weight1 = 0 then alphabetically x y else weight-weight1) tuplelst;;

let rec buildstr = function
| [] -> ""
| [(x, _)] -> x
| (x, _)::xs -> x^" "^buildstr xs;;

let orderWeight (s: string): string = buildstr (order(tuples (cut s)));;


(*first non repeating letter*)
let rec removeall n = function
| [] -> []
| x::xs -> if Char.uppercase_ascii x = n || Char.lowercase_ascii x = n then removeall n xs else x::removeall n xs;;


let first_non_repeating_letter s = if s = "" then None else
  let oglst = (s |> String.to_seq |> List.of_seq) in
let rec aux lst trimlst (n : char) = match lst with
| [] -> Some n
| x::xs -> if Char.uppercase_ascii x = n || Char.lowercase_ascii x = n then (if removeall n trimlst = [] then None else aux (List.tl (removeall n trimlst)) (removeall n trimlst) (List.hd (removeall n trimlst))) else aux xs trimlst n
in aux (List.tl oglst) (List.tl oglst) (List.hd oglst);;


(*snail*)

let array2d = [[1;2;3];
               [4;5;6];
               [7;8;9]];;

let array5d = [[1;2;3;4;5];
               [1;2;3;4;5];
               [1;2;3;4;5];
               [1;2;3;4;5];
               [1;2;3;4;5]];;

type currdir = L | R | T | B;;
let nextdir (dir : currdir) = 
  match dir with
  | R -> B
  | B -> L
  | L -> T
  | T -> R ;;

let get (x,y) array2d = List.nth (List.nth array2d x) y;;

let generate barrier =
let rec aux array dir (x,y) barrier = if List.length array = (barrier+1)*(barrier+1) then array else match dir with
| R -> if List.mem (x,y) array 
       then aux (array) (nextdir dir) (x+1, y-1) barrier 
       else if y = barrier
       then aux ((x,y)::array) (nextdir dir) (x+1, y) barrier
       else aux ((x,y)::array) (dir) (x, y+1) barrier
| B -> if List.mem (x,y) array 
       then aux array (nextdir dir) (x-1, y-1) barrier 
       else if x = barrier
       then aux ((x,y)::array) (nextdir dir) (x, y-1) barrier
       else aux ((x,y)::array) (dir) (x+1, y) barrier
| L -> if  List.mem (x,y) array
       then aux array (nextdir dir) (x-1, y+1) barrier
       else if y = 0
       then aux ((x,y)::array) (nextdir dir) (x-1, y) barrier
       else  aux ((x,y)::array) (dir) (x, y-1) barrier
| T -> if  List.mem (x,y) array
       then aux array (nextdir dir) (x+1, y+1) barrier
       else  aux ((x,y)::array) (dir) (x-1, y) barrier
      in aux [] R (0,0) (barrier);;

let rec translate lst array2d = match lst with
| [] -> []
| x::xs -> (get x array2d) :: (translate xs array2d)

let snail xs = if xs = [[]] then [] else List.rev (translate (generate (List.length xs-1)) xs);;


(*power set*)

let to_power_set set =
  if set = [] then [[]]
  else
    let set = set @ [-1] in
      let rec aux realset elem oldset newset =
        if realset = [] then newset
        else match oldset with
             | [] -> aux (List.tl realset) (List.hd realset) newset newset
             | x :: xs -> aux realset elem xs ((elem::x)::newset)
      in
      aux (List.tl (List.tl set)) (List.hd (List.tl set)) [[]; [(List.hd set)]] [[]; [(List.hd set)]];;

(*lazy list*)
type 'a llist = Cons of 'a * (unit -> 'a llist);;

let rec inat (x : int)  : int llist = Cons (x, fun () -> inat (x+1));;

let lfib () = let rec aux first second = Cons ((first+second), fun () -> aux (second) (first+second)) in Cons(0, fun () -> Cons(1, (fun () -> (aux 0 1))));;

let rec ltake (n : int) (llist : 'a llist) : 'a list = if n = 0 then [] else match llist with
| Cons(x, func) -> x:: (ltake (n-1) (func ()));;


let womp x = (x mod 2 = 1);;

let rec lfilter (func) (llist : 'a llist) : 'a llist = match llist with
| Cons(x, funct) -> if func x then Cons(x, fun () -> lfilter func (funct ())) else lfilter func (funct ());;



(*tree*)

type 'a tree = Node of 'a * 'a tree list

let rec maketree lst exclude = match lst with
| [] -> []
| x::xs -> if List.mem x exclude then maketree xs exclude else (Node (x, [])):: maketree xs exclude;;

let rec maptr (f) (tr : 'a tree list) = match tr with
| [] -> []
| x::xs -> (f x) :: (maptr f xs);;

let rec mapv (f) (tr : 'a tree list) = match tr with
| [] -> []
| x::xs -> (f x) @ (mapv f xs);;

let rec vlst tree = match tree with
| Node(x, tr) -> x::(mapv (vlst) tr);;

let update f tree =
let rec aux f exclude tree = match tree with
| Node (x, tr) -> if tr = [] then Node (x, maketree (f x) exclude) else Node (x , maptr (aux f exclude) tr) 
in aux f (vlst tree) tree;;