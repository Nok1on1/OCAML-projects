let rec revtuples lst = List.fold_left (fun (x) (y,z) -> x@ [(z,y)] ) [] lst;;

let rec oddeven lst = List.fold_left (fun x y -> if List.length lst mod 2 = 1 then (if List.length x mod 2 = 1 then x @ [y] else y::x) else (if List.length x mod 2 = 0 then x @ [y] else y::x) ) [] lst;;


type 'a custom_llist = (unit -> 'a custom_cell) and 'a custom_cell = NilC | ConsC of ('a * 'a custom_llist);;

type 'a ocaml_llist = 'a ocaml_cell Lazy.t and 'a ocaml_cell = NilO | ConsO of ('a * 'a ocaml_llist);;

(*Implement a mapping function that maps a function over a lazy list.
 Implement it both for custom and OCaml lazy list variants.
 Call them respectively map_over_custom_llist and map_over_ocaml_llist.*)

 let rec map f llist = match llist () with
 | ConsC (x, y) -> fun () -> ConsC((f x), map f y)
 | NilC -> fun () -> NilC;;


 let rec map f ocamlllist = Lazy.map f ocamlllist;;