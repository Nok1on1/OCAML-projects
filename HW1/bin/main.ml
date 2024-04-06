let rec arithemticsum x = if x = 1 then 1 else x+arithemticsum(x-1);;

let rec factorial x = match x with
| 1 -> 1
| _ -> x*factorial(x-1);;

type student = {
  first_name : string;
  last_name :string;
  id : int;
  semester : int;
  grades: (int * float) list;
  }

type database = student list

let addstudent (db : database) (x : student) = x :: db;;

let rec findstudent (db : database) (x : int) =
   match db with
  | [] -> raise Not_found
  | h::t -> if h.id == x then h else findstudent t x;;

let rec find_last_name (db : database) (ln : string) (ln_list : student list) =
  match db with
  | [] -> ln_list
  | h::t -> if h.last_name == ln then find_last_name t ln (h::ln_list) else find_last_name t ln ln_list;;

let rec remove (db : database) (id : int) (newdb : database) = 
  match db with
  | [] -> newdb
  | h::t -> if h.id == id then remove [] id newdb @ db else remove t id (h::newdb);;


let rec remove_first_name (db : database) (first_name : string) =
  match db with
  | [] -> []
  | h::t -> if h.first_name == first_name then remove_first_name t first_name else h::remove_first_name t first_name;;

let rec find_semester (db : database) (semester : int) =
  match db with
  | [] -> 0
  | h::t -> if h.semester == semester then 1 + find_semester t semester else find_semester t semester;;

let rec average (db : database) (id : int) = 
  let l = (findstudent db id).grades in 
    let rec average1 l (grades : float) (count : int) =
       match l with
          | [] -> if count = 0 then 0.0 else grades /. float_of_int count
          | (_h,h1)::t -> average1 t (grades+.h1) (count+1) in 
            average1 l 0.0 0;;



(*Mishmash*)
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

(*Ocamlfication*)
  let rec foo (x :int) (y : int) (b : bool) = 
    let rec foo1 x y b = 
      match (y-x) with
      | 0 -> x
      | _ -> if b then foo (x+1) (y) (false) else foo x (y+1) (true) 
    in
       if x>y then foo1 y x b  else foo1 x y b;;
       
(*polynomial party*)
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




(*Longest twins But with overlap*)
  let rec size n = function
  | [] ->  (n-1)
  | _::t -> size (n+1) t;;

  let rec cropleft lst n = 
    if n = 0 then [] else
       match lst with 
       | [] -> [] 
       | h::t -> h :: cropleft t (n-1);;


  let rec cropright lst n = match lst with
  | [] -> []
  | _h::t -> if n = 0 then lst else cropright t (n-1);;

  let allpossiblen lst n =
    let rec aux (lst : 'a list) lst1 (lst2 : 'a list list) n =
    match lst with
    | [] -> lst2
    | h::t -> aux t (cropright lst1 1 @ [h]) ([cropright lst1 1 @ [h]] @ lst2) n
  in aux (cropright lst n) (cropleft lst n) ([cropleft lst n]) n;;

  let compare lst = 
    let rec aux lst n =
      match lst with
      | [] -> []
      | h::t -> if h = n then h else aux t n 
    in
    let rec aux1 lst =
      match lst with
      | [] -> []
      | h::t -> if aux t h != [] then aux t h else aux1 t
    in aux1 lst;;

  let longtwins lst = 
    let rec aux lst n =
      match lst with
      | [] -> []
      | _ -> if compare (allpossiblen lst n) != [] then compare (allpossiblen lst n) else aux lst (n-1)
    in aux lst ((size 0 lst)-1);;