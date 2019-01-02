Compelling Higher Kinded Types, Type Classes in F#
====================================================



There is no parameterized module in F#, however, as the result of the existance of
some other power infrastructures, it's much easier for F# to express higher abstractions in terser methods.

The secret of the F#'s conciseness comes from the 2 parts.

- `Active Patterns <https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/active-patterns>`_ help to avoid write :code:`inj` and :code:`prj` manually.
    If you've never heard of :code:`inj` and :code:`prj` before, there is `a prerequisite article <./paper-reading-LHKP.html>`_ for you to
    figure out what they are and some necessary knowledge about Higher Kined Types and Type Classes.

    If you cannot get along with that prerequisite article,
    you might need to try Haskell language to learn about Functor and Monad to
    get some related basic knowledge.

- `Static Resolved Type Parameters <https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/generics/statically-resolved-type-parameters>`_


The following sections are made in Chinese, and if you cannot read them, `this repo <https://github.com/thautwarm/FSTan>`_ is for you to check how to implement
HKT and Type Classes.


Active Pattern
-------------------------------


- "临时的"Enum

.. code-block:: FSharp

    let (|IsWhatIWant|NotWhatIWant|) x =
        if x > 10 then IsWhatIWant
        else NotWhatIWant

    let checkNumber():
        match 10 with
        | IsWhatIWant -> failwith "fatal!"
        | _ -> ()

- "临时的"构造器

.. code-block:: FSharp

    let (|Dec|) x =
        if x > 0 then Some <| x - 1
        else None

    let test(): int option =
        match 1 with
        | Dec x -> x

- 模拟datatype

.. code-block:: FSharp

    let (|Just|Nothing|) (a: 'a option) =
        match a with
        | Some a -> Just a
        | None a -> Nothing

    let Just (a: 'a): 'a option = Some a
    let Nothing<'a>: 'a option = None

    // Now you can use `Just` and `Nothing` to replace
    // constructors `Some` and `None` of type `'a option`.

    let bind (m:'a option) (k: 'a -> 'b option):
        match m with
        | Nothing -> Nothing
        | Just x  -> k x

模拟datatype的功能看似鸡肋, 但在简化HKT的使用上将会非常重要。


更高更妙的F#
-----------------------------