type natural = Zero | Succ of natural;;

let integer_of_natural x=
let rec aux x count = 
match x with
| Zero -> count
| Succ y -> aux y (count + 1) 
  in aux x 0;;

let rec natural_of_integer x =
  match x with
  | 0 -> Zero
  | n -> (Succ (natural_of_integer (n-1)));;

let rec add x y =
    match x with
    | Zero -> if y = Zero then Zero else add y x
    | Succ n -> Succ (add n y);;

let mult x y =
let rec aux x y z =
  match x , y with
  | Succ n, y -> aux n y (add z y)
  | Zero, _ -> z 
in aux x y Zero;;
  

let expo x y =
  let rec aux x y z =
    match y with
    | Zero -> z
    | Succ y -> aux x y (mult z x)
  in aux x y (Succ Zero);;

let rec leq (x : natural) (y : natural) =
  match x , y with
  | (Succ n), (Succ m) -> leq n m
  | Zero, Zero -> true
  | Zero, _ -> true
  | _, Zero -> false