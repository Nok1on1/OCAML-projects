let rec arithemticsum x = if x = 1 then 1 else x+arithemticsum(x-1);;

let rec factorial x = match x with
| 1 -> 1
| _ -> x*factorial(x-1);;

type student = {
  first_name : string;
  last_name :string;
  id : int;
  semester : int;
  grades: (int * float) list;
  }

type database = student list

let addstudent (db : database) (x : student) = x :: db;;

let rec findstudent (db : database) (x : int) =
   match db with
  | [] -> raise Not_found
  | h::t -> if h.id == x then h else findstudent t x;;

let rec find_last_name (db : database) (ln : string) (ln_list : student list) =
  match db with
  | [] -> ln_list
  | h::t -> if h.last_name == ln then find_last_name t ln (h::ln_list) else find_last_name t ln ln_list;;

let rec remove (db : database) (id : int) (newdb : database) = 
  match db with
  | [] -> newdb
  | h::t -> if h.id == id then remove [] id newdb @ db else remove t id (h::newdb);;


let rec remove_first_name (db : database) (first_name : string) =
  match db with
  | [] -> []
  | h::t -> if h.first_name == first_name then remove_first_name t first_name else h::remove_first_name t first_name;;

let rec find_semester (db : database) (semester : int) =
  match db with
  | [] -> 0
  | h::t -> if h.semester == semester then 1 + find_semester t semester else find_semester t semester;;

let rec average (db : database) (id : int) = 
  let l = (findstudent db id).grades in 
    let rec average1 l (grades : float) (count : int) =
       match l with
          | [] -> if count = 0 then 0.0 else grades /. float_of_int count
          | (_h,h1)::t -> average1 t (grades+.h1) (count+1) in 
            average1 l 0.0 0;;