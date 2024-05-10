let to_power_set set =
  if set = [] then [[]]
  else
    let set = set @ [-1] in
      let rec aux realset elem oldset newset =
        if realset = [] then newset
        else match oldset with
             | [] -> aux (List.tl realset) (List.hd realset) newset newset
             | x :: xs -> aux realset elem xs ((elem::x)::newset)
      in
      aux (List.tl (List.tl set)) (List.hd (List.tl set)) [[]; [(List.hd set)]] [[]; [(List.hd set)]]
;;