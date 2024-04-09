type natural = Zero | Succ of natural
let lst = [1;2;3;4];;

let natural_of_integer x = Succ (x-1)