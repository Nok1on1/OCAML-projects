type tree = Empty
  | Node of int * tree * tree

let rec head (t : tree list) =
  match t with
  | Node(x, y, z)::_ -> Node(x, y, z)
  | Empty::_ | [] -> raise Not_found

let rec insert x t =
  match t with
  | Node(z, left, right) -> Node(z, insert x left, insert x right)
  | Empty -> x

let rec find x t =
  match t with
  | Node(num, left, right) ->
    if x = left || x = right then true
    else find x left || find x right
  | Empty -> false

let rec findmax t =
  let rec aux t maxt maxnum =
    match t with
    | Node(num, left, right) ->
  if num > maxnum then (aux left t num) @ (aux right t num)
  else (aux left maxt maxnum) @ (aux right maxt maxnum)
    | Empty -> [maxt]
  in
  let rec aux1 max t = function
    | [] -> max
    | Empty::xs -> aux1 max t xs
    | (Node(num, left, right))::xs ->
  if num > max then aux1 num (Node(num, left, right)) xs
  else aux1 max t xs
in (*mezareba imena yleobaa*)
  

let rec findmin t = 

let rec delete x t = 

let rec tolist t = 

let rec fromlist l = 

let rec height t =
