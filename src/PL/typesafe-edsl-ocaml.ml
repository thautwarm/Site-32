module type TNT = sig
  type 'a typed_name
  val inj : string -> 'a typed_name
  val prj : 'a typed_name -> string
end

module TN : TNT = struct
  type 'a typed_name = string
  let inj = fun x -> x
  let prj = fun x -> x
end

type 'a typed_name = 'a TN.typed_name
type 'a expr =
| Fun : 'b typed_name * 'a expr -> ('b -> 'a) expr
| Var : 'a typed_name  -> 'a expr
| Val : 'a -> 'a expr
| App : ('a -> 'b) expr * 'a expr -> 'b expr
| Let : 'a typed_name * 'a expr * 'b expr -> 'b expr

type 'a entry = {key : 'a typed_name;  value: 'a}
type _ hl =
| Nil : unit hl
| Cons : 'a entry * 'b hl -> ('a * 'b) hl


let rec lookup : type a b. a typed_name -> b hl -> a option = fun n ->
  function
  | Cons ({  key; value  }, _) when TN.prj key = TN.prj n -> Some (Obj.magic value)
  | Cons(_, tl) -> lookup n tl
  | _ -> None



let rec interp : type a ctx. ctx hl -> a expr -> a =
  fun ctx ->
  function
  | Fun(n, body) ->
    fun v -> interp (Cons({key = n; value=v}, ctx)) body
  | Val i -> i
  | Var n -> (match lookup n ctx with Some v -> v | _ -> raise Not_found)
  | Let(n, bound, body) ->
    let ctx = Cons({key=n; value=interp ctx bound}, ctx)
    in interp ctx body
  | App(f, arg) ->
    (interp ctx f) (interp ctx arg)

let ctx = Cons({key=TN.inj "a"; value=1}, Cons({key=TN.inj "b"; value="3"}, Nil))

let x = interp ctx @@ let a = TN.inj "a" in Fun(a, Var a)
let _ = print_int @@ x 1

(**  '_weak1 -> '_weak1 = <fun> *)
