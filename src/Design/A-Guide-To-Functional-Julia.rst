=============================
 A Guide To Functional Julia
=============================

`Julia <https://julialang.org/>`_ is a multi-paradigm language which is purely dynamic but supports optional typing.

As the introduction of some basic constructs like ``For-Loop``, ``While-Loop`` and ``If-Else`` and ``Function`` akin to
those of Python, R or MATLAB, Julia becomes simple and concise, and allows you to work fluently with a fraction of its language
features.

  .. image:: ./assets-April-3/4-langs.png

Although People seldom focus on Julia's language features other than its high performance(until April 2019), I'd show my point
of view of the true functional programming in Julia.


First-Class
------------------------------------

Almost all language constructs in Julia are *first-class*, upholding more flexible and advanced program composition.

There're quite a lot of people that has a prejudice against Python for the absense of multi-line lambdas. Although
it's pretty trivial and straightforward to compile multi-line lambdas into normal Python code objects, the frontend,
more concretely the mandatory indentation, comfines Python to the expressive power where statements are rigidly separate
from expressions and then *first-class* misses.

Julia seems to be over-*first-class*, for all constructs there are expressions and, syntactically there's no
restriction to compose arbitrary constructs. Definitions and computations are distinct from each other in major
functional languages, but each of both consists of the other.

.. code-block:: Julia

   [let y=f(x); (x, y) end for x in points]

   map(
     function map_fn(x)
       # do stuffs
     end,
     points
  )


  let bind_maybe(a, f) -> a == nothing ? f(a) : nothing,
      (|>) = bind_maybe

      init          |> a ->
      do1_stuffs(a) |> a ->
      ...           |> a ->
      ...           |> a ->
      ...
  end


Polymorphisms of Multiple Dispatch
---------------------------------------


Julia supports polymorphisms to achieve necessary abstractions and reduce self-repeating, though it
does stand apart from those pooular industrial languages.

Multiple dispatch looks like parametric polymorphism, but actually it's more than the latter.

.. code-block:: Julia

   function usum(x :: Vector{T}) where T
      s = zero(T)
      for each in x
         s += each
      end
      s
   end

Overloading is supported as well.

.. code-block:: Julia

   uzero(_ :: Type{Int})     = 0
   uzero(  :: Type{Float64}) = 0.0

However, multiple dispatch is more than what I listed above. In fact, type is exclusively
a specialized instance of immutable data, while you can make dispatches via immutable data
no matter whether it is a type or others.

.. code-block:: Julia

   struct Const{T}
   end

   flip(::Type{Const{:even}}) = Const{:odd}
   flip(::Type{Const{:odd}})  = Const{:even}

   f(::Type{Const{0}}) = Const{:even}
   f(::Type{Const{1}}) = Const{:odd}
   f(::Type{Const{N}}) where N = f(Const{N-1}) |> flip


With above codes, we can statically compute parity of numbers, just as expected.

.. code-block:: Julia

   julia> @code_warntype f(Const{2})

   Body::Type{Const{:even}}
     1 ─     return Const{:even}


Note that when multiple dispatch fails at static inferences, it'll behave as dynamic dispatch like Python's.


Full-Featured Macros
----------------------

Macro is one of the quite few ways to achieve code reuse, also the reason of why
some programmer can be thousands of times more efficient than others.

.. code-block:: Julia

  julia> macro gen_var(n :: Int, f)
         defs = [Expr(:(=), Symbol("var", i), :($f($i)))  for i in 1:n]
         esc(Expr(:block,  defs..., nothing))
       end

  @gen_var (macro with 1 method)

  julia> f(x) = x * 10 - 2
  f (generic function with 1 method)

  julia> @gen_var f (x * 10 - 2)

  julia> var1
  8

  julia> var2
  18

  julia> var3
  28


Macro, the Function from AST to AST
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Once you know macros are functions from ASTs to ASTs, there's no mystery to Julia macros.


.. code-block:: Julia

   macro f(x)
     println(x)
     :($x + 1)
   end

   @assert (@f 1) == 2

Above snippet shows a vivid example of Julia macros. Firstly ``macro`` keyword leads a definition of
macro transformation rule, and ``@f`` marks a callsite of corresonding macro.

You might ask why ``(@f 1) == 2``, for the return of macro ``f`` is supposed to be an AST, it seems
a bit magic that it equals to an integer ``2``.

Pay attention to the expression ``@assert (@f 1) == 2``. As the macro invocations are processed recursively
from the inside out, we should firstly process ``@f 1``.

.. code-block:: Julia

  (function f(x)
      println(x)
     :($x + 1)         =>  :(1 + 1)
  end) 1

Above step also write to stdio, when executing the AST to AST function ``f``, a.k.a macro ``@f``.

Next, as we has already got the output, an AST ``:(1 + 1)``, imagine that we displace ``@f 1`` by it in the preceding codes,
which produces ``@assert $(:(1 + 1)) == 2``, simplify it, we'll get ``@assert (1 + 1) == 2``.

You might ask why not ``@assert :(1 + 1) == 2``, good question, let's dig into it.

Think that what you return from a macro invocation is always a runtime AST, it will not
be transformed into codes to compile, so that the macro becomes useless at all.

However, if we "unquote" the macro return

.. list-table:: *Unquote* Rule
   :widths: 6, 6
   :header-rows: 1
   :align: left

   * - Quoted
     - Unquoted

   * - ``:(:(1 + 1))``
     - ``:(1 + 1)``

   * - ``:(1 + 1)``
     - ``1 + 1``

   * - ``quote 1 + 1 end``
     - ``1 + 1``

   * - ``quote $x + x end``
     - ``<x> + x``, where ``<x>`` stands for some computated expression.

   * - ``1``
     - ``1``

   * - ``[1, 2, 3]``
     - ``[1, 2, 3]``

Above table unveils the rules of AST interpolations, and obviously there's a law that
if we say an expression is quoted ``N`` times, it'll be interpolated as an expression
quoted ``max(0, N - 1)`` times.

Scope and Hygiene
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The scoping rules of macros are simple enough when you are under the point of view that
macros are functions from ASTs to ASTs.

.. code-block:: Julia

  julia> module A
    var = 0
    macro ma()
      quote
        var
      end
    end
  end

  julia> var = 5555

  julia> A.@ma
  0

  julia> using .A: @ma

  julia> @ma
  0

The first I'd present here is, the expression a macro return is evaluated by
the module where the macro's defined.

When a macro is expanding inside the local scope of a function, a concept called *hygiene* comes up
naturally.

.. code-block:: Julia

  macro assign_y(x)
     :(y = $x)
  end

  function f(x)
    @assign_y x
    y
  end

  f(1)

You might expect it works, but unfortunately it won't, and solely feed you with

.. code-block:: Julia

  ERROR: UndefVarError: x not defined
  Stacktrace:
  [1] f(::Int64) at ./REPL[6]:2
  [2] top-level scope at none:0

The reason why for this is, ast interpolations will be always preprocessed to make sure all
bare symbols(not boxed in QuoteNode or deeper quotation) are transformed into **mangled** names(a.k.a, *gensym*) that looks
a bit weird like ``##a#168``. Also, the reason why Julia does this is, to by default avoid generate new symbols visible
in current local context.

Just think about you want a macro to log the value just calculated:

.. code-block:: Julia

   macro with_logging(expr)
      quote
        a = $expr
        @info :logging a
        a
      end
   end


We don't transform the symbol ``a`` into something like ``##a#167``,  what if
you have already define ``a`` in your codes?

.. code-block:: Julia

  function x5(a)
      x2 = @with_logging 2a
      x3 = @with_logging 3a
      x2 + x3
  end

  my_func(1)

You can see that if macro ``with_logging`` didn't transform ``a`` written in macro body,
you'll get ``x5(1) == 8`` instead of ``x5(1) == 5``.

That's it, and we call this sort of macro the Hygienic Macros.

But there does have some context-sensitive cases for code generation, that you want to
share the context of multiple generator functions. A impressive example is
my `MLStyle <https://github.com/thautwarm/MLStyle.jl>`_, which I'm extremely proud of
for it has reached a high performant pattern matching compilation with the extensibility
I've dreamed about since I started programming.

In this case that people want to generate symbols that will contaminate scopes,
Julia provides an **escape** mechanism to avoid *gensym*.

.. code-block:: Julia

  macro assign_y(x)
     esc(:(y = $x))
  end

  function f(x)
    @assign_y x
    y
  end

  f(1) # 1

The previous code finally works after supplementing a ``esc`` invocation on returned AST.


Other Useful Knowledge for Julia Macros
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. ``@__MODULE__`` gets you current module.

2. When you want to control which module to evaluate a given AST, you can use ``moduleX.eval(expr)`` or
   ``@eval moduleX expr``.

3. Although we already know macros are functions, something need to be stressed is,
  there're 2 implicit arguments of a macro: ``__module__`` and ``__source__``. ``__module`` is
  the module you invoke the macro in, ``__source__`` is the line number node that denotes the number of the line
  you invoke the macro.


Limitation of Julia
-----------------------------------


Absence of Function Type
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Julia is an ideal language for quite many domains but, not for all.

For people who're used to functional programming languages, especially for
the groups that tilts the advanced type-based polymorphisms(type classes' instance resolution,
implicit type variables, higher-kinded-polymorphisms).

In academic areas of programming, types are essential. I've attempted a lot with my friends to emulate
those advanced type-based polymorphisms in Julia, but finally we noticed that without implicit inferences
on functions, only dynamic typing and multiple dispatch are far from being sufficient.

In Julia, each function has its own type which is a subtype of ``Function``.

In Haskell, the type signature of a function does help in semantics:

.. code-block:: Haskell










