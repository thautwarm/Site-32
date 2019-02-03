Type Classes
==================================================================

Distinct from the traditional interfaces from so-called OOP, there is a solution to
make abstractions on types(including higher kinded types, the same below) and separate assigning features to types from the type definitions.

About Abstractions
----------------------------------

One of the goal of abstractions is to make better reuse of codes, which could be acheived through polymorphisms.

Another goal of abstractions is to make programs safer, which could be acheived through static checking.

To meet these expectations,  the **Type Class** then came into being.

A well-known example of polymorphisms with type classes is :code:`Functor`, the polymorphic function :code:`map` could be applied
on :code:`Array`, :code:`List`, :code:`Maybe/Option`, etc.

.. code ::
    
    val map : forall {f, a, b, Functor f} -> (a -> b) -> f a -> f b
    val lst : List<int>
    val arr : Array<int>
    val m   : Maybe<int>
    let f x = x + 1
    let _ = map f lst
    let _ = map f arr
    let _ = map f m
    
In above codes, :code:`List, Array, Maybe` are all :code:`Functor` s.

The **Functor** is not a type or type constructor. In my point of view, a type class :math:`A` could be taken
as a set of attributes :math:`\{A_1, A_2, \cdots \}` that could be used to describe types, and if we say type :math:`a` is an instance of type class :math:`A`,
it's the same to say that :math:`a` has a set of attributes :math:`\{A_1, A_2, \cdots \}`. These attributes can be leveraged for so-called instance resolution,
which specializes the implementation/instance of a polymorphic function on its callsite.

Given a custom implementation of :code:`Functor` and its instance for :code:`List`,

.. code-block :: Haskell

    type List = [] # avoid confusing stuffs for newbies.

    class Functor f where
        map :: (a -> b) -> f a -> f b

    instance Functor (List)
        map :: (a -> b) -> List f -> List b
        map f = \case
                []   -> []
                x:xs -> f x : map f xs
    

Let's see how to perform instance resolution on the callsites of :code:`map` .

.. code ::

    map (+1) [1, 2, 3]

    1. map :: Functor f => (a -> b) -> f a -> f b
    2. [1, 2, 3] :: Num a => List a
    3. (+)       :: Num a => a -> a -> a
    4. 1         :: Num a => a
    
    5. for (3), (4), 
        (+1)     :: Num a => a -> a

    6. for (1), (2)
        f = List,
        now look up the instance (Functor List),
        find out the corresponding map function.
    
        map :: (a -> b) -> f a -> f b,
        where f = List, a = Num a => a
    
    7. for (6), (5),

        a -> b ~ Num a => a -> a
        b :: Num a => a
        (+1) :: Num a => a -> a

        p.s: `~` means `unify`
    
    8. as a result,
        
        map (+1) [1, 2, 3] :: Num a => List a
        
If we make the instance :code:`Functor A`, then we can use the polymorphic :code:`map` function
on type :code:`A`.

Something deserved to be note here is, although the traditional interfaces from OOP can provide polymorphisms,
it's much weaker when it comes to static checking. Type classes reach polymorphisms without casting objects,
while if you want to use methods of some OOP interface, an unnecessary cast is always required.

For type classes could be leveraged to implement full-featured interfaces and the real(not the emulated) type classes can be faster than
interfaces(type classes avoid some redundant runtime costs), we can safely make the conclusion that type classes are superior to OOP interfaces.
 

About Separation of Type Definitions and Data Manipulations 
---------------------------------------------------------------------------

OOP interfaces strongly couple the definitions with the valid interoperations for a type(for you have to point out which interfaces to be inherited), while
type classes provide the freedom upon this aspect.

For instance, a :code:`show` method is aimed at representing the objects of some type that implements type class :code:`Show`
with strings.

.. code-block :: Haskell

    data S = S
    instance Show S where
        show S = "oh, it's a S, the unique S!"

In traditional OOP, we have to define :code:`toString/ToString/__str__` methods exactly when defining the types, while languages with
type classes, like Rust and Haskell, would allow you to define it somewhere more proper. Something that is super advanced is,
you can make :code:`show` method qualified in current workload, and allow other implementations of :code:`show` outside the qualified scope.

This extremely enhanced the extensibility and enabled thorough uncoupling without any mental burden.

To illustrate the advantages, let's think about F#.

1. F# doesn't support statically resolved type parameters with type extensions, as a consequence, although it's feasible for F# to implement ad-hoc polymorphisms,
but in terms of some built-in datatypes and primitive types like :code:`int, List, Array`, etc., it's definitely impossible. A wrapper is always required,
and severe performance issues could be caused.

2. Let's focus on the :code:`show` again. The common method :code:`sprintf` is already defined and works with some existed protocols which should been ensured when definining
the your data types. The way to organize your codes is then fixed, of course the way to *sprintf* a datatype is then fixed, too.

.. code-block :: FSharp

    [<StructuredFormatDisplay("{str}")>] // This must be given here to make custom format method.
    type MyData = ... 