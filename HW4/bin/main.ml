let f1 lst (x,y) = lst@[(y,x)];;
let f2 (lst, i) x = if i mod 2 = 0  then x::lst else lst @ [x];;

let f3 acc (k, v) = 
  let g x = if x = k then v else acc x in
  g;;

let build_g lst =
  let g = List.fold_left f3 (fun _ -> 0) lst in
  g;;


let rec map f = function
  | [] -> []
  | x :: xs -> f x :: map f xs;;

let map_tr f lst =
  let rec aux f lst1 = function
  | [] -> lst1
  | x::xs -> aux f xs  ((f x)::lst1)
in aux f lst [];;


let rec replicate n x =
  if n < 1 then [] else x :: replicate (n-1) x

let replacte_tr n x =
  let rec aux n x lst = if n < 1 then lst else aux (n-1) x (x::lst) in aux n x [];; 


type 'a custom_llist = (unit -> 'a custom_cell)
  and 'a custom_cell = NilC | ConsC of ('a * 'a custom_llist);;

type 'a ocaml_llist = 'a ocaml_cell Lazy.t
  and 'a ocaml_cell = NilO | ConsO of ('a * 'a ocaml_llist)