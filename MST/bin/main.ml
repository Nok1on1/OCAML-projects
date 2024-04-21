type graph = (int * float * int) list;;

let rec first = function
| [] -> []
| (x,_,_)::_ -> [x];;

let second = function
| (_x,_w,y) -> y;;

let rec find y = function
| [] -> raise Not_found
| x::xs -> if x = y then true else find y xs;;

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

let union graph currentnodes =
  let rec aux (currentnodes : int list) (graph : graph) (edges : graph) =
    match currentnodes with
    | [] -> edges
    | x::xs -> aux xs graph (edges @ getedges graph x)
in aux currentnodes graph [];;

let removeedges currentnodes graph = 
  let rec aux (graph : graph) (edges : graph) (currentnodes : int list) =
    match edges with
    | [] -> []
    | (x,w,y)::xs -> if (find x currentnodes) && (find y currentnodes) then aux (remove graph (x,w,y)) xs currentnodes else aux graph xs currentnodes
  in aux graph (union graph currentnodes) currentnodes;;

let mst (graph : graph) = 
let rec aux (graph : graph)  (tree : graph) (currentnodes : int list) = 
match graph with
| [] -> tree
| _ -> aux (removeedges currentnodes graph) (skinny(union graph currentnodes) :: tree) ((second (skinny(union graph currentnodes)))::currentnodes)

in aux  graph [List.hd graph] (first graph);;