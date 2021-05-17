这是一篇未完成的草稿

```ocaml
let some_code = <fun a -> $( func <a> ) >
(** or 
let some_code = <let a = 1 in $( func <a> ) >
*)
```

Which means that the explicit source of symbol `a` must be given in the code.
This way, creating a variable will get you **a monomorphic scope**(while the type can be polymorphic), and `<a>` here will always stand for some variable created in advanced by `let a = ... ` or `fun a -> ...`, but not for those incoming ones.



## Flexible Scope for Meta Programming with Type Safety and Scope Safety

Meta programming that can be statically checked by compilers is very useful because code generation is quite
a real-world thing used in various fields. http://okmij.org/ftp/meta-programming/Shonan-challenge.pdf


```ocaml
let binary_op = fun left op right -> <$op $left $right>
```

You can find that the `op` here will be polymorphic.

`op` can be a code of `+`, then the type is `(int -> int -> int) code`.

Same way, `(float -> float -> float) code` and `(string -> string -> string) code` are also valid typing.

In fact, `binary_op` here has the type

```
let binary_op = fun eval left op right -> <$op ($eval $left) ($eval $right)>
```

Feature extractions for different kinds of data: 

1. rank significant discrete features. Such as the grade level: A, B, C, D or 1, 2, 3, 4.
2. rank insignificant discrete features. Such as the species: animal, insect, plant.
3. continuous features. Such as the species: animal, insect, plant.
4. above kinds of features with missing values

So we want to write a staged program to specialize the feature extraction for a given shape of dataset.


However, so far, most programming languages cannot support meta programming with type safety and scope safety
while keeping the expressiveness just like unhygienic macros in LISPs.


In Haskell, meta programming is always via Template Haskell, which is untyped.
In MetaOCaml, meta programming can be type-safe and scope-safe, however, composing code objects is a little bit verbose
and non-intuitive when comparing to LISPs.

$$
\mathbf{e} = \mathbf{l} \\; | \\; \mathbf{v} \\; | \\; \mathbf{e} \\; \mathbf{e} \\; | \\; \mathit{let} \\; \mathbf{v} \\; = \\; \mathbf{e} \\; \mathit{in} \\; \mathbf{e} \\; \\\\
   \\; \\; | \\; \\mathit{fun} \\; \\mathbf{v} \\; \rightarrow \\; \mathbf{e} \\; | \\; \$ \mathbf{e}  \\; |  \\;  \lt \mathbf{e} \gt
$$

Such a language can be expressed with MetaOCaml, and then the following expression does not compile

```ocaml
let some_code = < a >
```

This is because symbol `a` is not defined outside. The proper occurrence of `<a>` has to be something like

```ocaml
let bin = fun left op right -> <$op $left $right>

let some_code = <fun a -> $( func <a> ) >
(** or 
let some_code = <let a = 1 in $( func <a> ) >
*)
```

Which means that the explicit source of symbol `a` must be given in the code.
This way, creating a variable will get you **a monomorphic scope**(while the type can be polymorphic), and `<a>` here will always stand for some variable created in advanced by `let a = ... ` or `fun a -> ...`, but not for those incoming ones.

```ocaml
let is_even 
```

