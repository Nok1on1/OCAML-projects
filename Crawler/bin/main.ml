type tree =
| Empty
| Node of int * tree * tree;; (*value, left child, right child*)

type command = Left | Right | Top | New | Remove | Push | Replace;; 

type crawler =  tree list;;
type stack = tree list;;
type cmdslist = command list;;

let element (crawler : crawler)  =
match crawler with
| [] -> Empty
| x::_xs -> x;;

let movecleft (crawler : crawler) =
  match (element crawler) with
  | Empty -> crawler  
  | Node (_value, left, _right) -> left :: crawler;;


  let movecright (crawler : crawler) =
    match (element crawler) with
    | Empty -> crawler
    | Node (_value, _left, right) -> right :: crawler;;

let returntop (crawler : crawler) =
  match crawler with
  | [] -> crawler
  | _::xs -> xs;;

let newnode (crawler : crawler) =
  match crawler with
  | [] -> crawler
  | _::xs -> Node(0, Empty, Empty)::xs;;

let remove (crawler : crawler) = 
  match crawler with
  | [] -> raise Not_found
  | _x::xs -> (Empty) :: xs;;

let rec down (tree : tree) (cmdslist : cmdslist) = 
  match cmdslist, tree with
  | Left::xs , Node(value, left, right) -> Node(value, down left xs, right)
  | Right::xs, Node(value, left, right) -> Node(value, left, down right xs)
  | Top::xs, Node(value, left, right) -> Node(value, left, right)
  | ;;

let crawl (cmdslist : cmdslist) (crawler : crawler) = 
  let rec aux cmdslist crawler = match cmdslist with
  | Left::xs -> crawl xs (movecleft crawler)
  | Right::xs -> crawl xs (movecright crawler)
  | Top::xs -> crawl xs (returntop crawler)
  | New::xs -> crawl xs (newnode)
in aux cmdslist crawler;;