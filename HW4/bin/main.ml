let f1 lst (x, y) = lst @ [(y, x)];;

let f2 (lst, i) x =
  if i mod 2 = 0 then x :: lst else lst @ [x];;

let f3 acc (k, v) =
  let g x = if x = k then v else acc x in
  g;;

let build_g lst =
  let g = List.fold_left f3 (fun _ -> 0) lst in
  g;;

let rec map f = function
  | [] -> []
  | x :: xs -> f x :: map f xs;;

let map_tr f lst =
  let rec aux f lst1 = function
    | [] -> lst1
    | x :: xs -> aux f xs ((f x) :: lst1)
  in
  aux f [] lst;;

let rec replicate n x =
  if n < 1 then [] else x :: replicate (n - 1) x;;

let replacte_tr n x =
  let rec aux n x lst =
    if n < 1 then lst else aux (n - 1) x (x :: lst)
  in
  aux n x [];;

type 'a custom_llist = (unit -> 'a custom_cell)
and 'a custom_cell = NilC | ConsC of ('a * 'a custom_llist);;

type 'a ocaml_llist = 'a ocaml_cell Lazy.t
and 'a ocaml_cell = NilO | ConsO of ('a * 'a ocaml_llist);;

let rec map_over_custom_llist f (lst : 'a custom_llist) =
  match lst () with
  | NilC -> (fun () -> NilC)
  | ConsC (x, lst) -> (fun () -> ConsC ((f x), map_over_custom_llist f lst));;


  let rec map_over_ocaml_llist f (lst : 'a ocaml_llist) : 'b ocaml_llist = 
    lazy (
      match Lazy.force lst with
      | NilO -> NilO
      | ConsO (x, xs) -> ConsO (f x, map_over_ocaml_llist f xs)
    )



  (* Helper Functions *)

let rec from_to_custom from to_ step =
  if from <= to_ then
    fun () -> ConsC (from, from_to_custom (from + step) to_ step)
  else
    fun () -> NilC;;

let rec print_custom_llist n c_list =
  if n != 0 then
    match c_list () with
    | NilC -> print_string "Nil\n"
    | ConsC (h, t) ->
      Printf.printf "%d, " h;
      print_custom_llist (n - 1) t
  else
    print_string "...\n";;

let rec custom_llist_to_string n c_list =
  if n != 0 then
    match c_list () with
    | NilC -> "Nil"
    | ConsC (h, t) ->
      string_of_int h ^ ", " ^
      custom_llist_to_string (n - 1) t
  else
    "...";;

let rec from_to_ocaml from to_ step =
  if from <= to_ then
    lazy (ConsO (from, from_to_ocaml (from + step) to_ step))
  else
    lazy NilO;;

let rec print_ocaml_llist n o_list =
  if n != 0 then
    match Lazy.force o_list with
    | NilO -> print_string "Nil\n"
    | ConsO (h, t) ->
      Printf.printf "%d, " h;
      print_ocaml_llist (n - 1) t
  else
    print_string "...\n";;

let rec ocaml_llist_to_string n o_list =
  if n != 0 then
    match Lazy.force o_list with
    | NilO -> "Nil"
    | ConsO (h, t) ->
      string_of_int h ^ ", " ^
      ocaml_llist_to_string (n - 1) t
  else
    "...";;