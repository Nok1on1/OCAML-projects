module type Ring = sig
  type t
  val zero : t
  val one : t
  val add : t -> t -> t
  val mul :  t -> t -> t
  val compare : t -> t -> int
  val to_string : t -> string
end;;

module type Matrix = sig
  type elem
  type t
  val create : int -> int -> t
  val identity : int -> t
  val from_rows : elem list list -> t
  val set : int -> int -> elem -> t -> t
  val get : int -> int -> t -> elem
  val transpose : t -> t
  val add : t -> t -> t
  val mul : t -> t -> t
  val to_string : t -> string
end 