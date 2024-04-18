type tree =
| Leaf of int 
| Node of int * tree * tree;; (*value, left child, right child*)

type command = Left | Right | Top | Push

type path =  tree list;;

let element (path : path)  =
match path with
| [] -> Leaf 0
| x::_xs -> x;;

let movecleft (path : path) =
  match (element path) with
  | Leaf x -> path  
  | Node (_value, left, _right) -> left :: path;;


  let movecright (path : path) =
    match (element path) with
    | Leaf x -> path
    | Node (_value, _left, right) -> right :: path;;

let returntop (path : path) =
  match path with
  | [] -> raise Not_found
  | _::xs -> xs;;

let replace  (path : path) (node : tree) =
  match path with
  | [] -> raise Not_found
  | _::xs -> node::xs;;

let remove (path : path) = 
  match path with
  | [] -> raise Not_found
  | _x::xs -> (Leaf 0) :: xs;;

let push (path : path) (node : tree) =
  match path with
  | [] -> raise Not_found
  | x::xs -> 