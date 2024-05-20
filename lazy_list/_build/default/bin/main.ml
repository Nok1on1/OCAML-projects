type 'a llist = Cons of 'a * (unit -> 'a llist)


let lfib () = 
  let rec aux st nd = Cons(st, (fun () -> aux (nd) (st+nd))) 
  in aux 0 1;;

let rec ltake n = function
| Cons (x, xf) when n > 0 -> x :: ltake (n-1) (xf ())
| _ -> [];;

let rec lfilter funct = function
  | Cons(x, xf) when (funct x) -> Cons(x, fun () -> lfilter funct (xf ()))
  | Cons(_, xf) -> lfilter funct (xf ());;

let rec lmap funct = function
| Cons(x, xf) -> Cons(funct x , fun () -> lmap funct (xf ()) );;

type 'a custom_llist = (unit -> 'a custom_cell) and 'a custom_cell = NilC | ConsC of ('a * 'a custom_llist);;

type 'a ocaml_llist = 'a ocaml_cell Lazy.t and 'a ocaml_cell = NilO | ConsO of ('a * 'a ocaml_llist);;

(*Implement a mapping function that maps a function over a lazy list.
 Implement it both for custom and OCaml lazy list variants.
 Call them respectively map_over_custom_llist and map_over_ocaml_llist.*)

 let rec map f llist = match llist () with
 | ConsC (x, y) -> fun () -> ConsC((f x), map f y)
 | NilC -> fun () -> NilC;;


 let rec map f ocamlllist = Lazy.map f ocamlllist;;


 let rec merge llist1 llist2 = match llist1 (), llist2 () with
 | NilC, NilC -> fun () -> NilC
 | ConsC (x1, y1), ConsC (x2, y2) -> if x1 < x2 then fun () -> ConsC(x1, merge y1 llist2) else fun () -> ConsC(x2, merge llist1 y2)
 | NilC, _ -> llist2
 | _, NilC -> llist1;;


 let rec merge (llist1 : 'a ocaml_llist) (llist2 : 'a ocaml_llist) = match Lazy.force llist1, Lazy.force llist2 with
 | NilO, NilO -> lazy NilO
 | ConsO (x1, y1), ConsO (x2, y2) -> if x1 < x2 then lazy (ConsO (x1, (merge y1 llist2))) else lazy (ConsO(x2, merge llist1 y2))
 | NilO, _ -> llist2
 | _ , NilO -> llist1;;


 let rec drop_dupl_custom_llist llist x = 
  match llist () with
 | ConsC(num, next) -> if num = x then drop_dupl_custom_llist next num else fun () -> ConsC (num, drop_dupl_custom_llist next num)
 | NilC -> fun () -> NilC;;

 let rec drop_dupl_ocaml_llist llist x =  match Lazy.force llist with
 | ConsO(num, y) -> if num = x then drop_dupl_ocaml_llist y num else lazy ( ConsO(num, drop_dupl_ocaml_llist y num))
 | NilO -> lazy NilO;;


 let rec from_to_custom from to_ step =      
  if from <= to_      
    then fun () -> ConsC (from, from_to_custom (from + step) to_ step)      
      else fun () -> NilC

let rec print_custom_llist n c_list =
  if n != 0 then
    match c_list () with
    | NilC -> print_string "Nil\n"
    | ConsC (h, t) ->
        Printf.printf "%d, " h;
        print_custom_llist (n-1) t
  else
    print_string "...\n"

let rec custom_llist_to_string n c_list =
  if n != 0 then
    match c_list () with
    | NilC -> "Nil"
    | ConsC (h, t) ->
        string_of_int h ^ ", " ^ custom_llist_to_string (n-1) t
  else
    "..."

let rec from_to_ocaml from to_ step =
  if from <= to_ then
    lazy (ConsO (from, from_to_ocaml (from + step) to_ step))
  else
    lazy NilO

let rec print_ocaml_llist n o_list =
  if n != 0 then
    match Lazy.force o_list with
    | NilO -> print_string "Nil\n"
    | ConsO (h, t) ->
        Printf.printf "%d, " h;
        print_ocaml_llist (n-1) t
  else
    print_string "...\n"

let rec ocaml_llist_to_string n o_list =
  if n != 0 then
    match Lazy.force o_list with
    | NilO -> "Nil"
    | ConsO (h, t) ->
        string_of_int h ^ ", " ^ ocaml_llist_to_string (n-1) t
  else
    "..."
