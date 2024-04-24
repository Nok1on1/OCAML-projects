type tree =
  | Empty
  | Node of int * tree * tree;; (*value, left child, right child*)
  
  type command = Left | Right | Top | New | Remove | Push | Replace;; 
  
  type cmdslist = command list;;
  
  let element lst  =
  match lst with
  | [] -> Empty
  | x::_xs -> x;;

  let rec lastelement (lst : command list)= 
  match lst with
  | [] -> raise Not_found
  | [x] -> x
  | _::xs -> lastelement xs;;

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
    | x::xs -> if x = Top then aux xs [] (if lst1 = [] then lst2 @ [[x]] else lst2 @ [lst1] @ [[x]]) else
       if (x = New || x = Remove || x = Replace || x = Push) then aux xs [] (lst2 @ [(lst1@[x])]) else aux xs (lst1@[x]) lst2
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
    | Remove::_xs, _ -> Empty
    | Replace::_xs, _ -> element stack
    |_, _ -> raise Not_found;;

  let rec push (tree : tree) (cmdslist) =
    match cmdslist, tree with
    | Left::xs, Node(_,left,_) -> push left xs
    | Right::xs, Node(_,_,right) -> push right xs
    | Push::_xs, node -> node
    | _,_ -> raise Not_found;;
      
      let rec usecmds (cmdslistlist : command list list) (path : command list) (tree : tree) (stack : tree list) = 
    match cmdslistlist with
    | [] -> tree
    | x::xs ->  match x with
    | [] -> tree
    | [Top] -> usecmds xs (crop path) (tree) (stack)
    | lst -> if ispath lst then usecmds xs (path@lst) tree stack 
    else if (lastelement lst) = Push then usecmds xs (path@crop lst) tree ((push tree (path@lst)) ::stack)
    else if (lastelement lst) = Replace then  usecmds xs (path@crop lst) (down tree (path @ lst) stack) (tail stack)
    else  usecmds xs (path@crop lst) (down tree (path@x) stack) stack;;

  let crawl (cmdslist : cmdslist) (tree : tree) = 
    usecmds (separatecmds cmdslist) [] tree [];;  
