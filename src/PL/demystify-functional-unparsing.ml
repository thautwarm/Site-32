let lit : 'a. string -> (string -> 'a) -> 'a  =
  fun str ->
  fun k -> k str

let int : 'a. (string -> 'a) -> int -> 'a =
      fun k x -> k (string_of_int x)

let float : 'a. (string -> 'a) -> float -> 'a =
      fun k x -> k (string_of_float x)

let compose : 'a 'b 'c.
    ((string -> 'b) -> 'c) -> 
    ((string -> 'a) -> 'b) ->
    ((string -> 'a) -> 'c) =
      fun l r ->
      fun k ->
      l (fun sa -> r (fun sb -> k (sa ^ sb)))


let test =
  let (^) = compose in
  int ^ int ^ int ^ float ^ lit "2333" ^ int

let sprintf : ((string -> string) -> 'a)  -> 'a =
    fun op -> op (fun x -> x)
  
 let _ =
    let (^) = compose in
    print_endline @@
      sprintf
      (int ^ lit " + " ^ float ^ lit " = " ^ lit "?")
      1 2.0