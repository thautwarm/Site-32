# Some Use Cases for Higher Rank Polymorphisms(No Monad)

Higher rank polymorphism is regarded crucial mainly due to its necessity to
implement **monadic programming** infrastructure.

Monadic programming fundamentally enhances the programming composability,
by which your programs can always grow without invasive refactoring and
avoid repeating code.

However, monadic programming is not yet accepted by most programming communities,
so the higher rank polymorphism is still not honored much in general scope.

In this article we introduce a few "more practical" use cases for higher rank polymorphisms.

## Polymorphic Functions for eDSL

```ocaml
type 'a expr = ..
```

Does your `eDSL` have functions? Even if first-order ones?

```ocaml
module type TNT = sig
  type 'a typed_name
  val inj : string -> 'a typed_name
  val prj : 'a typed_name -> string
end

type 'a typed_name = 'a TN.typed_name
type 'a expr =
| Fun : 'b typed_name * 'a expr -> ('b -> 'a) expr
| Var : 'a typed_name  -> 'a expr
| Val : 'a -> 'a expr
| App : ('a -> 'b) expr * 'a expr -> 'b expr
| Let : 'a typed_name * 'a expr * 'b expr -> 'b expr

let ctx = Cons({key=TN.inj "a"; value=1}, Cons({key=TN.inj "b"; value="3"}, Nil))

val interp : type a ctx. ctx hl -> a expr -> a

let _ = interp ctx @@ let a = TN.inj "a" in Fun(a, Var a)

(* '_weak1 -> '_weak1 = <fun> *)
```

Implementation: [Gist Link](https://gist.github.com/thautwarm/080795ebc7d1c26d1e7f103ceb1ec1ca)

However, we cannot have a `('a. 'a -> 'a) expr` to represent polymorphic identity function,
due to

1. value restriction: cannot generalise from a let binding when the bound value is not a function
2. lack of higher rank types: cannot write `('a. 'a -> 'a) expr` in OCaml.

## Existential Types(Require Type Classes)

Existential types give us the power to properly specify constraints for your arguments.

```haskell
type Showable = forall a. Show a => a

x :: Showable
x = 1 -- 'Num a => Show a' exists

x :: Showable
x = "2" -- 'Show String' exists
```

In OOP languages, concepts and implementations for OOP interfaces are made for achieving the same.

The real existential types via higher rank polymorphisms are better than OOP interfaces because they work the same,
and the former needs no special language constructs from the language compiler.

## Unifiable Manipulation for Heterogeneous Data

Suppose you now have a dataframe with 3 columns in different data types.

Let's encode it in a static language like OCaml.

```ocaml
type _ df =
| EmptyDF : unit df
| JoinDF : 'a array * 'b df -> ('a * 'b) df

let dataframe =
  JoinDF( [|3; 2; 3|],
  JoinDF( [|"a"; "b"; "c"|],
  JoinDF( [|(1, 2); (3, 4); (5, 6)|],
  EmptyDF
  )))
```

There're many meaningful operations for the dataframe:

1. Drop/Skip several lines
2. Reorder/Randomize the rows
3. Delete/Insert a specific row
4. ...

**Note that those operations can be applied to each column, with a same function**.

```ocaml
let apply_columns f df = ??
(* val f : 'a array -> 'b *)


val reorder : int array -> 'a array -> 'a array
val randperm : int -> int array
let randomize : _ df -> 
let _ = apply_columns (reoder @@ randperm 3) dataframe
(* change the order of rows *)

let take n xs =
    if Array.length xs < n then
        xs
    else let ys = Array.make n (Obj.magic 0) in
         for i = 0 to n - 1 do
            ys.(i) <- xs.(i)
         done;
         ys
    
apply_columns (take 2) arr3
(* take the first 2 rows for all columns *)
```

Implementation: [Gist Link](https://gist.github.com/thautwarm/01ab69d4ae2420cd4ec6b7cb7607d425)

So far in OCaml, we can use a datatype to box a polymorphic function to partly achieve the higher rank polymorphisms.
