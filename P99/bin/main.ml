

let pack lst =
  let rec pack lst lst1 lst2 x =
    match lst with
    | [] -> lst1
    | h :: t -> if h = x then pack t lst1 (h::lst2) h else  pack t ((x::lst2)::lst1) [] h
  in pack lst [] [] " ";;