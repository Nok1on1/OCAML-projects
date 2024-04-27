let rec sub_sequences = function
  | [] -> []
  | x :: xs ->
      let rec sub_seq acc = function
        | [] -> []
        | y :: ys -> (y :: acc) :: sub_seq (y :: acc) ys @ sub_seq acc ys
      in
      sub_seq [x] xs @ sub_sequences xs

let anymatch lst x =
  let rec inn lst x n = match lst with
    | [] -> -1
    | y :: ys -> if x = y then n else inn ys x (n + 1)
  in
  inn lst x 0

let rec shorten lst idx = match lst, idx with
  | [], _ -> []
  | _, 0 -> []
  | _ :: xs, _ -> shorten xs (idx - 1)

let occurrences lst =
  let segs = sub_sequences lst in
  let rec count lst segs acc =
    match segs with
    | [] -> acc
    | x :: xs ->
        let rec aux l1 l2 n1 =
          match l2 with
          | [] -> 0
          | y :: ys -> if anymatch l1 y > 0 then aux (shorten l1 (anymatch l1 y)) ys (n1 + 1) else aux l1 ys n1
        in
        let occurrences = aux lst x 0 in
        if occurrences = 2 then count lst xs ((x, occurrences) :: acc) else count lst xs acc
  in
  count lst segs []

let longesttwins occ = 
  let rec aux ls n = 
    match ls with 
    |[] -> n 
    |(x,_)::xs -> if List.length x > List.length n then aux xs x else aux xs n 
  in 
    match occ with 
    [] -> []
    | (_, _):: ys -> aux ys []
let listi = [2; 3; 4; 5; 6; 2; 3; 4; 77; 7; 7];;