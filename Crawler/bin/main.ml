
type tree =
  | Empty
  | Node of int * tree * tree;; (*value, left child, right child*)
  
  type command = Left | Right | Top | New | Remove | Push | Replace;; 
  
  type stack = tree list;;
  type cmdslist = command list;;
  
  let element lst  =
  match lst with
  | [] -> Empty
  | x::_xs -> x;;

  let tail = function
  | [] -> []
  | _::xs -> xs;;

  let rec crop = function
  | [] -> []
  | [_] -> []
  | x::xs -> x::crop xs;;

  let separatecmds cmdslist = 
  let rec aux cmdslist lst1 lst2 =
    match cmdslist with
    | [] -> lst2
    | x::xs -> if x = Top || x = Push then aux xs [] ([lst1] @ [[x]] @ lst2) else
       if (x = New || x = Remove || x = Replace) then aux xs [] (lst2 @ [(lst1@[x])]) else aux xs (lst1@[x]) lst2
  in aux cmdslist [] [];;

  let rec ispath = function
  | [] -> true
  | x::xs -> if x != Left && x != Right then false else ispath xs;; 

  let rec down (tree : tree) (cmdslist : cmdslist) (stack : tree list) = 
    match cmdslist, tree with
    | [] , _ -> Empty
    | Left::xs , Node(value, left, right) -> Node(value, (down left xs stack), right)
    | Right::xs, Node(value, left, right) -> Node(value, left, down right xs stack)
    | New::_xs, Node(_, _, _) -> Node(0, Empty, Empty)
    | Push::xs, Node(_,_,_) -> down tree xs ([tree]@stack)
    | Remove::_xs, _ -> Empty
    | Replace::_xs, _ -> element stack
    |_, _ -> raise Not_found;;

  let rec usecmds (cmdslistlist : command list list) (path : command list) (tree : tree) (stack : tree list) = 
    match cmdslistlist with
    | [] -> tree
    | x::xs ->  match x with
    | [] -> tree
    | [Top] -> usecmds xs (tail path) (tree) (stack)
    | [Push] -> usecmds xs path (down tree (path@[Push]) stack)  stack
    | lst -> if ispath lst then usecmds xs (path@lst) tree stack else usecmds xs path (down tree (path@x) stack) stack
    ;;

  let crawl (cmdslist : cmdslist) (tree : tree) = 
    usecmds (separatecmds cmdslist) [] tree [];;
    let tree : tree = (Node(1, Node(2, Node(3, Empty, Empty), Empty), Node(4, Empty, Empty)))


(*let crawl (cmdslist : cmdslist) (crawler : crawler) = 
  let rec aux cmdslist crawler = match cmdslist with
  | Left::xs -> crawl xs (movecleft crawler)
  | Right::xs -> crawl xs (movecright crawler)
  | Top::xs -> crawl xs (returntop crawler)
  | New::xs -> crawl xs (newnode)
in aux cmdslist crawler;;*)


(*let movecleft (crawler : crawler) =
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
  | _x::xs -> (Empty) :: xs;;*)