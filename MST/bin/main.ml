type graph = (int * float * int) list;;

let rec first = function
| [] -> []
| (x,_,_)::_ -> [x];;

let rec find y = function
| [] -> raise Not_found
| x::xs -> if x = y then true else find y xs;;

let getedges (graph : graph) x = 
  let rec aux (graph : graph) vertice (lst : graph) =
    match graph with
    | [] -> lst
    | (x,w,y)::tail -> if x = vertice || y = vertice then aux tail vertice ((x,w,y)::lst) else  aux tail vertice lst 
  in aux graph x [];;

let skinny (graph : graph) =
  let rec aux (edge : int * float * int) = function
  | [] -> edge
  | (x,w,y)::xs -> match edge with
  | (_,w1,_) -> if w1 > w then aux (x,w,y) xs else aux edge xs 
in aux  (List.hd graph) (List.tl graph);;

let union graph nodes =
  let rec aux (nodes : int list) (graph : graph) (edges : graph) =
    match nodes with
    | [] -> edges
    | x::xs -> aux xs graph (edges @ getedges graph x)
in aux nodes graph [];;

let remove nodes graph =
let rec aux graph nodesedges nodes newgraph =
  match nodesedges with
  | [] -> newgraph 
  | x::xs -> if find x graph then aux graph nodesedges nodes newgraph else aux graph nodesedges nodes (x::newgraph) in
  aux graph (union graph nodes) nodes [];;


let mst (graph : graph) = 
let rec aux (graph : graph)  (tree : graph) (currentnodes : int list) = 
match tree with
| [] -> raise Not_found
| (x,_,y)::xs -> aux graph (skinny((aux (x::currentnodes) graph [])) :: tree ) (y::currentnodes)

in aux  graph [] (first graph);;