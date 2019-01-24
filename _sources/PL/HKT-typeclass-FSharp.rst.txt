Compelling Higher Kinded Types and Type Classes in F#
===========================================================



There is no parameterized module in F#, however, as the result of the existance of
some other power infrastructures, it becomes much easier for F# to express higher abstractions tersely.

The secret of the F#'s conciseness comes from the following 2 parts.

- `Active Patterns <https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/active-patterns>`_ help to avoid write :code:`inj` and :code:`prj` manually.
    If you've never heard of :code:`inj` and :code:`prj` before, there is `a prerequisite article <./paper-reading-LHKP.html>`_ for you to
    figure out what they are and some necessary knowledge about Higher Kined Types and Type Classes.

    If you cannot get along with that prerequisite article,
    you might need to try Haskell language to learn about Functor and Monad to
    get some related basic knowledge.

- `Static Resolved Type Parameters <https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/generics/statically-resolved-type-parameters>`_


Some of the following sections are made in Chinese, and if you cannot read them, `this repo <https://github.com/thautwarm/FSTan>`_ is for you to check how to implement
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


上面是Active pattern的一些示例, 不完整但足以表达重点:

**允许在解构数据时嵌入额外逻辑**

这将是之后用来实现LHKT的重点。


Reference Reading

-----------------------------


[1] Haskell 中有着类似Active Pattern的扩展: `Haskell ViewPatterns <https://ghc.haskell.org/trac/ghc/wiki/ViewPatterns>`_.

[2] Haskell ViewPatterns vs F# Active Patterns: `View vs Active Pattern <https://mail.haskell.org/pipermail/haskell-cafe/2009-January/053643.html>`_


Statically Resolved Type Parameters
-----------------------------

In the language reference of F#, statically resolved type parameters are not well documented, but you might want to `check it <https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/generics/statically-resolved-type-parameters>`_ firstly.

Here I'm to bring more cases to tell you how to take advantage of it.

The first case is about structural typing.

    | In structural typing, an element is considered to be compatible with another if,
    | for each feature within the second element's type, a corresponding and identical feature exists in the first element's type.[3]


So let's imagine a case that we want to make some functions for all objects/types that have a
common specific behavior.

Let's think about the sound of animals, which could be taken as the behavior.

Firstly, let's define some sounds.

.. code-block :: FSharp

    type Sound = Ooooh | Uhhh | Ahhh | QAQ | Woke | Wa | Ding | Miao

Then we define some intuitive active patterns to make natural-language-like notations:

.. code-block :: FSharp

    let (|LessThan|_|) param value =
        if value > param then Some ()
        else None

    // 1 is less than 20
    assert (match 1 with LessThan 20 -> true | _ -> false)

.. code-block :: FSharp

    type CatSpecies = ScottishFold | RussianBlue | MeoGirl
    type Cat = {
        weight   : int
        species  : CatSpecies
    } with
        static member sound: Cat -> Sound =
            function
            | {weight = LessThan 20; species = ScottishFold} -> Miao
            | {weight = LessThan 40; species = RussianBlue} -> Uhhh
            | {weight = LessThan 100; species = RussianBlue} -> Wa
            | {weight = LessThan 100} -> QAQ
            | {species = MeoGirl} -> Ding

    type Color = Black | Brown | White
    type Scale = {height : int, thickness : int}
    type Dog = {
        color   : Color
        scale   : Scale
    } with
        static member sound : Dog -> Sound =
            function
            | {height = LessThan 30; thickness = LessThan 10} -> Wa
            | {height = LessThan 30} -> Woke
            | {height = LessThan 50} -> Ooooh
            | _ -> failwith "a fat wolf detected"

Now I want to a function, which takes an animal as input, and output its sound.

For we didn't use the discriminated union/ADT to represent animals, how can we make
this polymorphic function?

Here you are:

.. code-block :: FSharp

    let sound(a : ^a when ^a: (static member sound: ^a -> ^b)) =
        (^a (static member sound: ^a -> ^b) a)

    assert sound {weight = 100; species = MeoGirl} = Ding
    assert sound {height = 40; thickness = 15} = Ooooh


Another case could be more formal and quite related to our topic.

To be continue.


Reference

-----------------------------

[3] https://en.wikipedia.org/wiki/Structural_type_system#Description
