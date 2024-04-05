let rec interleave lst1 lst2 lst3 oglst =
  let rec interleave2 lst1 lst2 oglst =
    match lst1, lst2 with
    | [], _ -> oglst @ lst2
    | _, [] -> oglst @ lst1
    | h::t, x::xs -> interleave2 t xs (oglst @ [h;x])
    in
  match lst1, lst2, lst3 with
  | [], [], [] -> oglst
  | [], _, _ -> interleave2 lst2 lst3 oglst
  | _, [], _ -> interleave2 lst1 lst3 oglst
  | _, _, [] -> interleave2 lst1 lst2 oglst
  | h::t, x::xs, y::ys -> interleave t xs ys (oglst @ [h;x;y]);;


  let rec foo (x :int) (y : int) (b : bool) = 
    let rec foo1 x y b = 
      match (y-x) with
      | 0 -> x
      | _ -> if b then foo (x+1) (y) (false) else foo x (y+1) (true) 
    in
       if x>y then foo1 y x b  else foo1 x y b;;

  let eval_poly (x :float) (lst : float list) =
    let rec size n = function
    | [] ->  (n-1)
    | _::t -> size (n+1) t 
  in 
  let rec aux x (lst : float list) n =
    match lst with
    | [] -> raise Exit
    | [x] -> x
    | h::t -> h*.(x ** (float_of_int n)) +. aux x t (n-1)
  in
  aux x lst (size 0 lst);;

  let rec size n = function
  | [] ->  (n-1)
  | _::t -> size (n+1) t

  let rec firstn lst n = if n = 0 then [] else match lst with | [] -> [] | h::t -> h :: firstn t (n-1);;
  let rec crop lst n = match lst with
  | [] -> []
  | _h::t -> if n = 0 then lst else crop t (n-1);;

  let allpossiblen lst n =
    let rec aux lst lst1 (lst2 : 'a list list) n =
    match lst with
    | [] -> lst2
    | h::t -> aux t (h :: firstn lst1 (n-1)) ((h :: firstn lst1 (n-1)) @ lst2) n
  in aux (crop lst n) (firstn lst n) (firstn lst n) n;;


