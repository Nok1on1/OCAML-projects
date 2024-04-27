let rec miss list n = 
  match list with 
  | _::t -> if n = 0 then t else miss t (n - 1)
  | [] -> [];;

let rec take list n = 
  match list with 
  | h::y -> if n = 0 then [] else h::take y (n - 1)
  | [] -> [];;

let rec size list = 
  match list with 
  | _::t -> 1 + size t
  | [] -> 0;;

let get tuple = 
  let rec getunia list li =
    match li with 
    | _::t -> if list = take li (size list) then Some list else getunia list t 
    | [] -> None
  in 
  match tuple with 
  | (g, j) -> getunia g j;;

let rec tuples list n = 
  match list with 
  | _::t -> if size list <= (2 * n) then (take list n, miss list (n-1)) :: [] else (take list n, miss list (n-1)) :: tuples t n 
  | [] -> [];;

let rec whole_lists list n = 
  if n > 1 then tuples list n @ whole_lists list (n - 1) else [];;

let longest_twins list = 
  let whole = whole_lists list (size list / 2) in
  let rec inner listunia = 
    match listunia with 
    | h::t -> 
      begin 
        match get h with
        | Some x -> x 
        | None -> inner t 
      end
    | [] -> []
  in 
  inner whole;;

let listi = [2; 3; 4; 5; 6; 2; 3; 4; 77; 7; 7];;