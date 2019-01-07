type ('a, 't) app

module type Mappable = sig
  type t
  val map: ('a -> 'b) -> ('a, t) app -> ('b, t) app
end

module Common =
struct
  type t
  external inj : 'a -> 'b = "%identity"
  external prj : 'a -> 'b = "%identity"
end

module type App = sig
  type 'a s
  type t
  external inj : 'a s -> ('a, t) app = "%identity"
  external prj: ('a, t) app -> 'a s  = "%identity"
end

type 'a mappable_impl = (module Mappable with type t = 'a)

module ListApp : App with type 'a s = 'a list = struct
  type 'a s = 'a list
  include Common
end

module MapList : Mappable with type t = ListApp.t = struct
  type t = ListApp.t
  let map (f: 'a -> 'b) (ca: ('a, t) app): ('b, t) app =
    let ca = ListApp.prj ca
    in let cb = List.map f ca
    in ListApp.inj cb
end


module ArrayApp : App with type 'a s = 'a array = struct
  type 'a s = 'a array
  include Common
end

module MapArray : Mappable with type t = ArrayApp.t = struct
  type t = ArrayApp.t
  let map (f: 'a -> 'b) (ca: ('a, t) app): ('b, t) app =
    let ca = ArrayApp.prj ca
    in let cb = Array.map f ca
    in ArrayApp.inj cb
end


let map (type t) (m: t mappable_impl) (f: 'a -> 'b) (a: ('a, t) app) =
      let module M = (val m : Mappable with type t = t) in
      M.map f a

let lst_data = [1; 2; 3]
let arr_data = [|1; 2; 3|]
let lst_data_hkt = ListApp.inj lst_data
let arr_data_hkt = ArrayApp.inj arr_data

let lst_mapped : (int, ListApp.t) app = map (module MapList) (fun x -> x + 1) lst_data_hkt
let arr_mapped : (int, ArrayApp.t) app = map (module MapArray) (fun x -> x + 1) arr_data_hkt

let () =  List.iter print_int (ListApp.prj lst_mapped); print_string "\n"
let () =  Array.iter print_int (ArrayApp.prj arr_mapped); print_string "\n"


type ('a, 'b) either = Left of 'a | Right of 'b
module EitherApp (Q: sig type t end): App with type 'a s = (Q.t, 'a) either = struct
  type 'a s = (Q.t, 'a) either
  include Common
end

module MapEither (Q: sig type t end): Mappable with type t = EitherApp(Q).t = struct
  module EitherApp2 = EitherApp(Q)
  type t = EitherApp2.t
  let map (f: 'a -> 'b) (ca: ('a, t) app) : ('b, t) app =
      let ca = EitherApp2.prj ca
      in let cb =
        match ca with
        | Left  l -> Left l
        | Right r -> Right (f r)
      in EitherApp2.inj cb
end

module IntModule = struct
  type t = int
end

  let either_data1_hkt =
    let module M = EitherApp(IntModule) in M.inj (Left 1)

  let either_data2_hkt =
    let module M = EitherApp(IntModule) in M.inj (Right 2)

  let either_map e =
    let module Data = EitherApp(IntModule)
    in let module Map  = MapEither(IntModule)
    in let res = map (module Map) (fun x -> x + 1) e
    in Data.prj res

  let either_mapped1 = either_map either_data1_hkt
  let either_mapped2 = either_map either_data2_hkt

  let show_either_int_int e =
      match e with
      | Left l  -> "Left " ^ string_of_int l
      | Right r -> "Right " ^ string_of_int r

  let () = print_string (show_either_int_int either_mapped1 ^ "\n")
  let () = print_string (show_either_int_int either_mapped2 ^ "\n")

