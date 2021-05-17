# Some Use Cases for Higher Rank Polymorphisms(No Monad)

Higher rank polymorphism is considered critical mainly due to its necessity to
implement **monadic programming**.

Monadic programming fundamentally enhances the programming composability,
by which your programs can always grow without invasive refactoring and
avoid repeating similar code.

However, monadic programming is not yet accepted by most programming communities,
so the higher rank polymorphism is still not honored much in the general scope.

In this article, we introduce a few "more practical" use cases for higher rank polymorphisms.

## Polymorphic Functions for eDSL

```ocaml
type 'a expr = ..
```

Could your `eDSL` hold functions? Moreover, think about polymorphic ones?

`eDSL` can be implemented in OCaml as follow. Such an `eDSL` is powered by a subset of OCaml's own type system. (You can access the full implementation via this [GitHub Link](https://gist.github.com/thautwarm/080795ebc7d1c26d1e7f103ceb1ec1ca))

```ocaml
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

let ctx = Cons({key=TN.inj "a"; value=1}, Cons({key=TN.inj "b"; value="3"}, Nil))

val interp : type a ctx. ctx hl -> a expr -> a

(* we try to write an identity function in the eDSL *)
let _ = interp ctx @@ let a = TN.inj "a" in Fun(a, Var a)

(* '_weak1 -> '_weak1 = <fun> *)
```

However, we **cannot** have a `('a. 'a -> 'a) expr` to represent a **polymorphic** identity function,
due to:

1. lack of higher rank types: we cannot write `('a. 'a -> 'a) expr` in OCaml.
2. (not critical)value restriction: cannot generalise from a let binding when the bound value is not a function

## Existential Types

This is related to Type Classes.

Existential types promise the power to properly specify constraints for your arguments.

```haskell
type Showable = forall a. Show a => a

x :: Showable
x = 1 -- 'Num a => Show a' exists

x :: Showable
x = "2" -- 'Show String' exists
```

In OOP languages, concepts and implementations for OOP interfaces are made for achieving the same.

The real existential types via higher rank polymorphisms are better than OOP interfaces because

1. they work the same way
2. the approach via HKP needs no special language constructs from the language compiler.

## Manipulation for Heterogeneous Data

Dataframe is a common concept in the scope of data analysis, however, they are always encoded with an unsafe approach:

**The data type of each column is validated at runtime, while it's possible to check them statically.**

For a concrete example about statically validating data, you may want to refer to [F# Type Provider](https://docs.microsoft.com/en-us/dotnet/fsharp/tutorials/type-providers/).

Higher rank polymorphisms will greatly enrich the operations on statically validated data. We will introduce this in the following content.

Now, suppose you have a dataframe with 3 columns in different data types: 
1. `int`
2. `string`
3. a tuple type `(int, int)`

Let's encode this in OCaml, which is a static language .

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

There are several common operations on dataframes:

1. Drop/skip several lines
2. Reorder/randomize the rows
3. Delete/insert a specific row
4. ...

Think about how to achieve them with a statically validated dataframe?


**As data are stored column by column**:
1. when you want to drop/skip several lines, you have to **skip the same positioned elements for all columns**.
2. when you want to randomize/reorder the rows, you have to **randomize/reorder all columns to the same order**.
3. when you want to delete/insert the rows, you have to **delete/insert the same positioned rows for all columns**.

A common abstraction of them is `apply_columns`, which applies the same function to different columns.

```ocaml
let apply_columns f df = ??
(* val f : 'a array -> 'b *)
```

However, the function is an argument, could an argument function apply to 3 columns in different types? 

The answer is NO for most generic programming languages.

This is because, without higher rank types, **an argument function cannot be polymorphic inside the function body.**:

```ocaml
let rec apply_columns f df =
  match df with
  | EmptyDF -> EmptyDF
  | JoinDF(col, tail) -> JoinDF(
      f col,  (* <- This is impossible!!!! *)
      apply_columns tail
    )
```

In the above code, `f col` is invalid, because `f` losses its polymorphism inside `apply_columns` and cannot accept the data `col`, which has a rigid polymorphic type instead of a monotype.

This is the reason why most generic programming languages cannot encode type-safe dataframes.

Using a trick to avoid impredicative polymorphisms can work the code. The full implementation can be found at this [GitHub Link](https://gist.github.com/thautwarm/01ab69d4ae2420cd4ec6b7cb7607d425). The following code shows the idea:

```ocaml
type apply = { f : 'a. 'a array -> 'a array }
val apply_columns : type a. apply -> a df -> a df
val reorder : int array -> 'a array -> 'a array
val randperm : int -> int array
let randomize  = apply_columns {f = reoder @@ randperm 3}
let take n xs =
    if Array.length xs < n then
        xs
    else let ys = Array.make n (Obj.magic 0) in
         for i = 0 to n - 1 do
            ys.(i) <- xs.(i)
         done;
         ys

apply_columns {f = take 2} arr3
(* take the first 2 rows for all columns *)
```