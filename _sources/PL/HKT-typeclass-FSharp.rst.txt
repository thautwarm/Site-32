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

- `Static Resolved Type Parameters <https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/generics/statically-resolved-type-parameters>`_ help to support resolution for implicit arguments.
    That's the way to achieve something similar to modular implicits in OCaml.


FYI, `this repo <https://github.com/thautwarm/FSTan>`_ provides an actual implementation.


Active Pattern
-------------------------------

- Temporary enumerations

.. code-block:: FSharp

    let (|IsWhatIWant|NotWhatIWant|) x =
        if x > 10 then IsWhatIWant
        else NotWhatIWant

    let checkNumber():
        match 10 with
        | IsWhatIWant -> failwith "fatal!"
        | _ -> ()

- Temporary recognizers

.. code-block:: FSharp

    let (|Dec|) x =
        if x > 0 then Some <| x - 1
        else None

    let test(): int option =
        match 1 with
        | Dec x -> x


- Simulating datatypes

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


Above snippets are about some use cases of Active Patterns, which might not be exhaustive but sufficent for today's topic:

**Allow to inject extra logics when destructuring data**,

subsequently, which is the key to implement LHKT in F#.


Statically Resolved Type Parameters
----------------------------------------------------------

In the language reference of F#, statically resolved type parameters are not well documented, but you might want to `check it <https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/generics/statically-resolved-type-parameters>`_ firstly.

Here I'm to bring more cases to tell you how to take advantage of it.

The first case is about structural typing.

    | In structural typing, an element is considered to be compatible with another if,
    | for each feature within the second element's type, a corresponding and identical feature exists in the first element's type.[3]


So let's imagine a case that we want to make some functions for all objects/types that have a
common specific behavior.

Let's think about the sound of animals, which could be taken as the behavior.

Firstly, let's define the enumerate some sounds:

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
        (^a : (static member sound: ^a -> ^b) a)

    assert sound {weight = 100; species = MeoGirl} = Ding
    assert sound {height = 40; thickness = 15} = Ooooh


Another case could be more formal and quite related to our topic.

Firstly, to represent higher kinded types, we use

.. code-block :: FSharp
    
    type hkt<'K, 'T> = interface end


Here, the :code:`hkt<'K, 'T>` is something that emulates the application of type constructor :code:`'K` on :code:`'T`, and
the kind ascription of :code:`'K` is(at least) :code:`* -> *` (e.g.,
a concrete type :code:`List<int>` is the application of type constructor :code:`List` on type :code:`int`, so semantically it could be written as :code:`hkt<List, int>`).

If any problem, refer to `this prerequisite article <./paper-reading-LHKP.html>`_.

Now, we're to make some type constructors to substitute the type variable :code:`'K`, like :code:`List`, 
:code:`Array`, :code:`Maybe` / :code:`Option`, etc. These type constructors(also types) can have some
common features, e.g., could be applied with a :code:`map` function, 

.. code-block :: FSharp

    type 'a Maybe = 'a Option
    let map_maybe : ('a -> 'b) -> 'a Maybe -> 'b Maybe = ..
    let map_list  : ('a -> 'b) -> 'a List  -> 'b List  = ..
    let map_array : ('a -> 'b) -> 'a Array -> 'b Array = ..


which could extracted as a useful concept, the functor. The functor is a **Type Class**, 
which is introduced to make abstractions on types(including the higher kinded types).

So, how can we emulate the functor, in another words, how can we assign the expected features to the corresponding type constructors
(:code:`List`, :code:`Array`, etc.)?

I'd like to directly give a solution below that was figured out on my own:

.. code-block :: FSharp

    [<AbstractClass>]
    type functor<'F>() =
        abstract member fmap<'a, 'b> :
            ('a -> 'b) -> hkt<'F, 'a> -> hkt<'F, 'b>

Now you might have lots of questions about above codes,

1. How can it help with implementing type classes?

2. Why we have to use :code:`AbstractClass` instead of ML Style records, or interfaces that're more lightweighted?

3. Why we should use such a shape :code:`functor<'F>` instead of straightforward :code:`functor` ?

I'll answer above questions in the following sections, but in terms of this section's topic, I should answer the first one here.

Firstly we need some infrastructures:

.. code-block :: FSharp

    let inline wrap<'o, ^f, 'a when ^f : (static member wrap : 'o -> hkt<'f, 'a>)> 
        (o: 'o) : hkt< ^f, 'a> =
        (^f : (static member wrap : 'o -> hkt<'f, 'a>) o)

    let inline unwrap<'o, ^f, 'a when ^f : (static member unwrap : hkt<'f, 'a> -> 'o)> 
        (f : hkt< ^f, 'a>) : 'o =
        (^f : (static member unwrap : hkt<'f, 'a> -> 'o) f)

    [<GeneralizableValue>]
    let getsig<'a> :'a = failwith "" // the implementation would be given subsequently.


Then we make an instance of :code:`functor` for :code:`List`.

.. code-block :: FSharp

    type mkList<'L>() =

        inherit functor<mkList<'L>>()

            static member wrap<'a> (x : List<'a>): hkt<mkList<'L>, 'a> =
                {wrap = x} :> _
            static member unwrap<'a> (x : hkt<mkList<'L>, 'a>): List<'a> =
                (x :?> _).wrap

            override member __.fmap<'a, 'b> (f : 'a -> 'b) (m : hkt<mkList<'L>, 'a>) : hkt<mkList<'L>, 'b> =
                List.map f <| unwrap m |> unwrap

    and listData<'L, 'a> =
        {wrap : List<'a>}
        interface hkt<mkList<'L>, 'a>

    let fmap<'a, 'b, 'F when 'F :> functor<'F>> :
        ('a -> 'b) -> hkt<'F, 'a> -> hkt<'F, 'b> =
        getsig<'F>.fmap

Now we can use :code:`fmap` on instances of :code:`hkt<mkList<'L>, 'a>` where :code:`'L <: mkList<'L>`.

.. code :: FSharp

    type MyList() =
        inherit mkList<MyList>()
        // you can interface other type classes here.
    type MyList = mkList<MyList>

    assert [2, 3, 4] = (
            let lst : hkt<MyList, _> = wrap [1, 2, 3]
            in unwrap <| fmap (fun x -> x + 1) lst
    )

Also the :code:`Maybe` functor:

.. code-block :: FSharp

    type mkMaybe<'M> =

        inherit functor<mkMaybe<'M>>()

            static member wrap<'a> (x : Option<'a>): hkt<mkMaybe<'M>, 'a> =
                {wrap = x} :> _
            static member unwrap<'a> (x : hkt<mkMaybe<'M>, 'a>): Option<'a> =
                (x :?> _).wrap

            override member __.fmap<'a, 'b> (f : 'a -> 'b) (m : hkt<mkMaybe<'M>, 'a>) : hkt<mkMaybe<'M>, 'b> =
                let m = unwrap m
                wrap <|
                match m with
                | Some m -> Some <| f m
                | None -> None


    type MyMaybe() =
        inherit mkMaybe<MyMaybe>()
        // you can interface other type classes here.

    type MyMaybe = mkMaybe<MyMaybe>

    
    assert Some 2 = (
        let m : hkt<MyMaybe, _> = wrap <| Some 1
        in unwrap <| fmap (fun x -> x + 1) m)

Something deserved to be mentioned here is, we are capable of simplifying this via active patterns:

.. code-block :: FSharp

    let Just<'M, 'a> (a: 'a) : hkt<mkMaybe<'M>, 'a> = wrap <| Some a

    [<GeneralizableValue>]
    let Nothing<'M, 'a> : hkt<mkMaybe<'M>, 'a> = wrap <| None

    let (|Just|Nothing|) (m: hkt<mkMaybe<'M>, 'a>) =
        let s: 'a Option = unwrap m
        match s with
        | Some m -> Just m
        | None    -> Nothing

    assert Some 2 = (
        let m : hkt<MyMaybe, _> = Just 1
        in unwrap <| fmap (fun x -> x + 1) m
    )
    assert 2 = match Just 2 with
               | Just a -> a
               | _ -> failwith ""

Now we have introduced how to use active patterns and static resolved type parameters to implement type classes,
but there are some problem remained here need to be answered.

The *getsig* Function and Implicits
-------------------------------------


The :code:`getsig` function makes it more convenient for F# to achieve type classes than OCaml(without modular implicits[4]).

With a glance of the polymorphic :code:`map` in OCaml, which is implemented in `this article <./paper-reading-LHKP>`_:

.. code-block :: OCaml


    type 'a mappable_impl = (module Mappable with type t = 'a)
    
    let map (type t) (m: t mappable_impl) (f: 'a -> 'b) (a: ('a, t) app) =
        let module M = (val m : Mappable with type t = t) in
        M.map f a

    
    let lst_mapped : (int, ListApp.t) app = map (module MapList) (fun x -> x + 1) lst_data_hkt
    let arr_mapped : (int, ArrayApp.t) app = map (module MapArray) (fun x -> x + 1) arr_data_hkt

We could find that the explicit argument :code:`m : t mappable_impl` of :code:`map` is redundant. In fact, it could be
inferred through the latter argument :code:`a : ('a, t) app`. If there is a way in OCaml to automatically create an instance
typed :code:`t mappable_impl` from the given argument :code:`a : ('a, t) app`, then it could reach the presence of F#.

Finally, I'd present an implementation of :code:`getsig` here:

.. code-block :: FSharp

    open System
    open System.Reflection

    let private ts  = Array.zeroCreate<Type> 0

    [<GeneralizableValue>]
    let getsig<'a> =
        let t = typeof<'a>
        let f = t.GetConstructor(
                    BindingFlags.Instance ||| BindingFlags.Public,
                    null,
                    CallingConventions.HasThis,
                    ts,
                    null)
        let o = f.Invoke([||])
        o :?> 'a


Why AbstractClass?
--------------------------------------

This is required for we need

1. Default implementations of type classes
2. Inheritances among type classes(such as :code:`Monad <: Applicative <: Functor`)

These two features are necessary, if you have any problem about this point, please check `Functor.fs <https://github.com/thautwarm/FSTan/tree/master/FSTan/Functor.fs>`_, 
`Applicative.fs <https://github.com/thautwarm/FSTan/tree/master/FSTan>`_, and `Monad.fs <https://github.com/thautwarm/FSTan/tree/master/FSTan>`_.

So the questions above could be answered here:

**Q**:Why cannot we use records instead of abstract classes?

**A**:Records(in F#) conflict against type classes' Inheritances.



**Q**:Why cannot we **always** use interfaces instead of abstract classes?

**A**:Currently there is no support for interfaces with default methods in F#. I've heard that C# 8 has supported this feature, but it hasn't been introduced into F# yet.


Moveover, there is a related story about **Traits**, which is a concept equivalent to type classes. 
Type classes could be implemented more smoothly via interfaces with default methods, which is leveraged by Scala language[5].


Why *Functor<'F>* instead of the straightforward *Functor*
-----------------------------------------------------------------------------------------------------


You might have thought of this:

.. code-block :: FSharp

    [<AbstractClass>]
    type functor() =
        abstract member fmap<'a, 'b, 'F <: functor> :
            ('a -> 'b) -> hkt<'F, 'a> -> hkt<'F, 'b>


There is something wrong with above codes.

Firstly, it mixes up inheritances among type classes with making instances of type classes.

If we have :code:`List <: functor`, and :code:`Applicative <: functor`, isn't it saying :code:`Applicative` is in the same
category as the :code:`List`?

Furthermore, static resolved type parameters cannot tell you which interface/abstract class your type has extended with:

.. code :: FSharp

    type mkList() =
        inherit Functor()
        // ...
    
    let fmap<'a, 'b, 'F when 'F :> functor> :
        ('a -> 'b) -> hkt<'F, 'a> -> hkt<'F, 'b> =
        getsig<'F>.fmap
    

The :code:`getsig` used above is impossible to implement in F#.

:code:`'F when 'F :> functor` doesn't work with instance resolution, for the F# compiler won't make assumptions that :code:`'F` has the abstract methods of :code:`functor`, but
:code:`'F when 'F :> functor<'F>` is okay.

Therefore, we should use :code:`functor<'F>`.



References and Further Reading
-----------------------------------------------------------

[1] Haskell ViewPatterns: https://ghc.haskell.org/trac/ghc/wiki/ViewPatterns

[2] Haskell ViewPatterns vs F# Active Patterns: https://mail.haskell.org/pipermail/haskell-cafe/2009-January/053643.html

[3] Structural-Typing: https://en.wikipedia.org/wiki/Structural_type_system#Description

[4] OCaml Modular Implicits : https://discuss.ocaml.org/t/modular-implicits/144

[5] Scala compiles traits to interfaces with default methods: https://github.com/scala/scala/pull/5003
