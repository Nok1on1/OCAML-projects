let rec interleave lst1 lst2 lst3 lst4 =
  match lst1 lst2 lst3 with
  | pattern -> pattern
  | [], [], [] -> lst4
  | [], _, _ -> let rec 