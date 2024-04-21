type graph = (int * float * int) list;;

let first = function
| [] -> []
| (x,_,y)::_ -> [x;y];;


let rec find y = function
| [] -> false
| x::xs -> if x = y then true else find y xs;;

let addnode currentnodes = function
| (x,_w,y) -> if not (find x currentnodes) then x::currentnodes else
     if not (find y currentnodes) then y:: currentnodes else currentnodes;;

let remove graph y =
let rec aux y newgraph = function
| [] -> newgraph
| x::xs -> if y = x then aux y newgraph xs else aux y (x::newgraph) xs 
in aux y [] graph;;

  
let getedges (graph : graph) x = 
  let rec aux (graph : graph) vertice (lst : graph) =
    match graph with
    | [] -> lst
    | (x,w,y)::tail -> if x = vertice || y = vertice then aux tail vertice ((x,w,y)::lst) else  aux tail vertice lst 
  in aux graph x [];;

let skinny (edges : graph) =
  let rec aux (edge : int * float * int) = function
  | [] -> edge
  | (x,w,y)::xs -> match edge with
  | (_,w1,_) -> if w1 > w then aux (x,w,y) xs else aux edge xs 
in aux  (List.hd edges) (List.tl edges);;
let rec remove_duplicates lst =
  match lst with
  | [] -> []
  | h :: t -> if List.mem h t then remove_duplicates t else h :: remove_duplicates t;;
  
  let union graph currentnodes =
  let rec aux (currentnodes : int list) (graph : graph) (edges : graph) =
    match currentnodes with
    | [] -> remove_duplicates edges
    | x::xs -> aux xs (graph) (edges @ getedges graph x)
in aux currentnodes graph [];;

let removeedges currentnodes graph = 
  let rec aux (graph : graph) (edges : graph) (currentnodes : int list) =
    match edges with
    | [] -> graph
    | (x,w,y)::xs -> if (find x currentnodes) && (find y currentnodes) then aux (remove graph (x,w,y)) xs currentnodes else aux graph xs currentnodes
  in aux graph (union graph currentnodes) currentnodes;;

let mst (graph : graph) = 
let rec aux (graph : graph)  (tree : graph) (currentnodes : int list) = 
  match graph with
  | [] -> tree
  | _ -> aux (removeedges currentnodes graph) (skinny(union graph currentnodes) :: tree) (addnode currentnodes (skinny(union graph currentnodes)))
in aux  graph [] (first graph);;

(* Test cases for type graph *)
let graph1 : graph = [(1, 2.0, 3); (2, 3.0, 3); (3, 4.0, 5)];;
(*let graph2 : graph = [(1, 3.,4); (4, 3.,8); (8, 3.,7); (7, 2.,6); (8, 7.0,6); (6, 4.0,5); (5, 1.,3); (2, 2.,3); (1, 5.,2); (1, 7.,3); (5, 1.,4)];;

 Print the output of mst graph1 *)
(* Test cases for function mst *)
assert (mst graph1 = [(1, 2.0, 3); (2, 3.0, 4); (3, 4.0, 5)]);;


