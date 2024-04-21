type 'a llist = Cons of 'a * (unit -> 'a llist)

let lfib () = 
  let rec aux st nd = Cons(st, (fun () -> aux (nd) (st+nd))) 
  in aux 0 1;;

let rec ltake n = function
| Cons (x, xf) when n > 0 -> x :: ltake (n-1) (xf ())
| _ -> [];;

let rec lfilter funct = function
  | Cons(x, xf) when funct x -> Cons(x, fun () -> lfilter funct (xf ()))
  | Cons(_, xf) -> lfilter funct (xf ())