let sum lst = List.fold_left (fun x y -> x+y) 0 lst;;
let prod lst = List.fold_left (fun x y ->x*y) 1 lst;;

let max lst =  List.fold_left (fun x y -> if y > x then y else x) (List.hd lst) lst;;

let rev lst = List.fold_left (fun x y -> y@x) [] lst;;

let find a lst = List.fold_left  (fun x y -> x || (a = y)) false lst;;
let concat lst = List.fold_left (fun x y -> x^y) "" lst;;

let convert lst = List.fold_left (fun x y -> x@[(string_of_int y)]) [] lst;;

let filter f lst = List.fold_left (fun x y -> if (f y) then x @ y else x) [] lst;;


let length lst = List.fold_left (fun x _ -> x+1) 0 lst;;
let longest lst = List.fold_left (fun x y -> if (length y) > (length x) then y else x) [] lst;;

let revtuple lst = List.fold_left (fun x (a,b) -> x@[(b,a)] ) [] lst;;


module type  GeometryUtilSig = sig
  type shape
  type point

  val rectangle_area : point -> point -> float
  val rectangle_perimeter : point -> point -> float
  val circle_area : float -> float
  val circle_perimeter : float -> float

  val triangle_area : point -> point -> point -> float
  val triangle_perimeter : point -> point -> point -> float

end


module GeometryUtil : GeometryUtilSig = struct
  type shape = Rectangle | Circle | Triangle
  type point = float * float

  let distance ((x1,y1) : point)  ((x2,y2) : point) =
    let dx = x2 -. x1 in
    let dy = y2 -. y1 in
    sqrt(dx *.dx +. dy *. dy)

  let rectangle_area ((x1,y1) : point)  ((x2,y2) : point) = 
    let width = abs_float(x1-.x2) in
    let height = abs_float(y1 -. y2) in
    width *. height

  let rectangle_perimeter ((x1,y1) : point)  ((x2,y2) : point) = 
    let width = abs_float(x1-.x2) in
    let height = abs_float(y1 -. y2) in
    2. *.(width +. height)
  
  let circle_area r = let pi = 3.14 in pi *. r *. r
  let circle_perimeter r = let pi = 3.14 in 2. *. pi *. r
  let triangle_area ((x1,y1) : point)  ((x2,y2) : point) ((x3,y3) : point)  =  (0.5) *. (x1 *. (y2 -. y3) +. x2 *. (y3 -. y1 ) +. x3 *. (y1 -. y2))

  let triangle_perimeter ((x1,y1) : point)  ((x2,y2) : point) ((x3,y3) : point) = 
    let ab = distance (x1,y1) (x2,y2) in
    let bc = distance (x2,y2) (x3,y3) in
    let ac = distance (x3,y3) (x1,y1) in
    ab +. bc +. ac 
  
  end