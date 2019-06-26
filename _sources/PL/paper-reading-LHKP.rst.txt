Paper Reading: Lightweight-Higher-Kinded-Polymorphism
==========================================================

`PDF <https://www.cl.cam.ac.uk/~jdy22/papers/lightweight-higher-kinded-polymorphism.pdf>`_ is available online.


How-To
----------------------

In fact, the technique (hereafter as **LHKP** ) introduced in this paper works perfectly only when
the order of types is less than 2 (have kind :code:`*` or :code:`* -> *`) and type
constructor is not an endofunctor.

LHKP achieves first order types through a parametric interface type :code:`type ('a, 't) app`, denoted as
:code:`'a 't` in higher kinded ML, where :code:`'a` is applicated to :code:`'t` where :code:`'t` has kind
:code:`* -> *`.

.. code-block :: OCaml

    type ('a, 't) app
    module type App = sig
        type 'a s
        type t
        external inj : 'a s -> ('a, t) app = "%identity"
        external prj: ('a, t) app -> 'a s  = "%identity"
    end

For any problem with :code:`%identity`,
check `%identity in OCaml <https://stackoverflow.com/questions/8482624/ocaml-identity-function>`_.

We can then make intuitive explanations using the implementation of polymorphic :code:`map`.

.. code ::

    val map: forall t a b {t <: mappable} .(a -> b) -> t a -> t a

Firstly, present the common part for all type constructors/applications (:code:`App`) here:

.. code-block :: OCaml

    module Common = struct
        type t
        external inj : 'a -> 'b = "%identity"
        external prj : 'a -> 'b = "%identity"
    end


Then declare a *type class* :

.. code-block :: OCaml

    module type Mappable = sig
        type t
        val map: ('a -> 'b) -> ('a, t) app -> ('b, t) app
    end

    type 'a mappable_impl = (module Mappable with type t = 'a)

Interesting, we've just implemented a type class tersely just like in Haskell.

Next, let's implement the :code:`list` type constructor:

.. code-block :: OCaml

    module ListApp : App with type 'a s = 'a list = struct
        type 'a s = 'a list
        include Common
    end

Now we've almost achieved the final goal, and what's left to do is still similar to Haskell for
we're exactly going to implement type class :code:`Mappable` for type constructor :code:`list`:

.. code-block :: OCaml

    module MapList : Mappable with type t = ListApp.t = struct
        type t = ListApp.t
        let map (f: 'a -> 'b) (ca: ('a, t) app): ('b, t) app =
        let ca = ListApp.prj ca
        in let cb = List.map f ca
        in ListApp.inj cb
    end

And finally, we got a polymorphic :code:`map`:

.. code-block :: ocaml

    let map (type t) (m: t mappable_impl) (f: 'a -> 'b) (a: ('a, t) app) =
      let module M = (val m : Mappable with type t = t)
      in M.map f a

    let () =
        let lst = ListApp.prj(map (module MapList) (fun x -> x + 1) (ListApp.inj [1; 2; 3]))
        in List.iter print_int lst

We can also use the same :code:`map` to work with :code:`Array` type constructor:

.. code-block :: OCaml

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

Now, we can show our polymorphic functions:

.. code-block :: OCaml

    let lst_data = [1; 2; 3]
    let arr_data = [|1; 2; 3|]
    let lst_data_hkt = ListApp.inj lst_data
    let arr_data_hkt = ArrayApp.inj arr_data

    let lst_mapped : (int, ListApp.t) app = map (module MapList) (fun x -> x + 1) lst_data_hkt
    let arr_mapped : (int, ArrayApp.t) app = map (module MapArray) (fun x -> x + 1) arr_data_hkt

    let () =  List.iter print_int (ListApp.prj lst_mapped); print_string "\n"
    let () =  Array.iter print_int (ArrayApp.prj arr_mapped); print_string "\n"


That's all about the core secrets of implementing type classes and HKT with this approach.



Static Resolution: More polymorphic
----------------------------------------


Take care that, in last section, the so-called **LHKP** is also kind of disgusting because we have to give the type constructor manually.

.. code ::

     map (module MapList) ...
     map (module MapArray) ...


It's obvious that, type constructors like :code:`MapList` and :code:`MapArray` had already been
given in other arguments:

.. code::

    val map: type t. (t mappable_impl) ('a -> 'b) -> ('a, t) app -> ('b, t) app

In fact, the argument typed :code:`t mappable_impl` is unique given a unique :code:`t`
(in other words, :code:`mappable_impl` is injective).
So how about create an instance of type :code:`t mappable_impl` automatically from the type :code:`t` ?

If we can do this, we then have a better polymorphism no worse than Haskell's.

.. code ::

    val arr_data: ('a, ArrayApp.t) app
    val lst_data: ('a, ListApp.t) app
    val f: 'a -> 'b

    map f lst_data: ('b, ListApp.t) app
    map f arr_data: ('b, ArrayApp.t) app

Yes, that's possible, actually we could use type variable inside a generic function/value to
instantiate the type constructor.

Another obscure thing is that, type variable deciding the instantiation of
type constructor might not appear in parameters, return type can also be used to do such things.


This kind of instantiation could be implemented through the static resolution/static duck typing,
which is provided by F# language, empowering us to use almost full-featured type classes and higher kinded types.

This technique would be introduced in this article: `更高更妙的F# <./HKT-type class-FSharp.html>`_.

For OCaml alternatives, check `modular implicits <http://tycon.github.io/modular-implicits.html>`_ .

Limitation1: Much Higher Kinded
------------------------------------------

When it comes much higher kinded types(like :code:`* -> * -> *`),
in haskell it's notable trivial:

.. code-block:: Haskell

    data Either a b = Left a | Right b

However, in many polupar ML languages like OCaml and F#, we have to use

.. code-block:: OCaml

    type ('a, 'b) either = Left of 'a | Right of 'b

    module EitherApp (Q: sig type t end): App with type 'a s = (Q.t, 'a) either = struct
        type 'a s = (Q.t, 'a) either
        include Common
    end

Then :code:`Either Int` in haskell can be written in OCaml as

.. code-block:: OCaml

    module IntModule = struct
        type t = int
    end

    module EitherInt = EitherApp(IntModule)

Let's implement :code:`Functor` class for :code:`forall a. Either a` :

.. code-block:: OCaml

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

However, using such :code:`Either` could be quite annoying:

.. code-block:: OCaml

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

That sucks so much for the lack of modular implicits and, it's an internal property that
expressing much higher kinded types (whose kind ascription is more complex than :code:`* -> *`)
requires many other boilerplate codes like :code:`('c, ('b, 'a) app) app`.

Limitation2: Identity
-------------------------

The second problem could be a little sensitive in the senarios where identity type constructor is in need.

A vivid example is :code:`MonadTrans`, for the consistency between :code:`StateT` and :code:`State`.

.. code-block:: OCaml


    newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
    type State s a = StateT s Identity a
    type Identity a = a

The problem occurs at :code:`s -> m (a, s)`, when :code:`m = Identity`, :code:`m (a, s) = (a, s)`.

More concretely, in OCaml, :code:`('a * 's,  identity) app` cannot become :code:`'a * 's` directly, which means that
an extra :code:`prj` is required here.

Then the implementation of :code:`State s a` cannot be equivalent to
Haskell, for we have to manually perform :code:`prj` each time after invoking :code:`runState/runStateT`.

Why this Lightweight-Higher-Polymorphism instead of the Haskell approach
--------------------------------------------------------------------------



According to the authors' arguments, an awkward scene in OCaml is, type aliases and actual type signatures cannot be
distinguished from each other, which makes it impossible to directly perform unification
after introducing higher kined types.

.. code-block :: ocaml

    'a cons ~ 'e 't

Now we don't know if :code:`cons` is a type constructor or simply an alias,
so we cannot say :code:`'a ~ 'e` and :code:`cons ~ 't`, in case :code:`'cons`
is a type alias like

.. code-block :: ocaml

    type 'a cons = ('a * 'a) list

where :code:`'a cons ~ 'e 't` should imply :code:`('a, 'a) ~ 'e` and
:code:`list ~ 't`.


So why not process type aliases firstly and convert them into
regular types containing no aliases?
The paper said "Since OCaml cannot distinguish between data types and aliases...",
I think it's not true, for OCaml types are named with lowercase leading character,
while datatype constructor are given through names that start with uppercase character.

.. code-block :: ocaml

    type <datatype> = | A of type
    type 'a <alias1> = 'a list
    type <alias2> = int