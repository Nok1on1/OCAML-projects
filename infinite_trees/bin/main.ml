type 'a ltree = LNode of 'a * (unit -> 'a ltree) * (unit -> 'a ltree);;

type 'a tree = Empty | Node of 'a * 'a tree * 'a tree;;

let rec layer_tree r = LNode (r, (fun () -> layer_tree (r+1)), (fun () -> layer_tree (r+1)));;

let rec interval_tree l h = LNode((l,h), (fun () -> (interval_tree l ((l+h)/2))), (fun () -> (interval_tree ((l+h)/2)) l));;

let rec rational_tree n d = LNode((n,d), (fun () -> (rational_tree n (d+1) )), (fun () -> (rational_tree (n+1) d) ));;

let rec top n t = match t with
| LNode (value, left, right) when n > 0 -> Node(value, (top (n-1) (left ())), (top (n-1) (right ())))
| LNode (value, _, _) -> Node(value, Empty, Empty);;

let rec map f t =
  match t with
  | LNode(value, left, right) -> LNode((f value), (fun () -> map f (left ())), (fun () -> map f (right ())));;

let rec find p t =
  match t with
  | LNode(value, left, right) ->
    if p value then Some t
    else
      let leftResult = find p (left ()) in
      let rightResult = find p (right ()) in
      match (leftResult, rightResult) with
      | (Some lv, _) -> Some lv
      | (_, Some rv) -> Some rv
      | _ -> None;;
  let optiontoltree (t : 'a  ltree option) = match t with
  | Some x -> x
  | None -> raise Not_found;;