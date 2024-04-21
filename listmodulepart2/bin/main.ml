let squaresum (lst : int list) = List.fold_left (fun x y -> x + y*y) 0 lst;;

let float_list (lst : int list) = List.map (fun x -> float_of_int x) lst;;

let to_string (lst : int list) = List.fold_left (fun x y -> x^string_of_int(y)) "" lst;;

let part_even (lst : int list) = (List.filter (fun x -> x mod 2 = 0) lst)@(List.filter (fun x -> x mod 2 = 1) lst);;