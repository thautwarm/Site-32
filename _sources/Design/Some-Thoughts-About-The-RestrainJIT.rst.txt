
=================================================================
Some Thoughts About The Restrain JIT
=================================================================

Yesterday is the PyConChina 2019, and I made a talk about the JIT stuffs. Actually,
about my recent project, the `Restrain Python JIT`_ .

In this project I did a backend-independent IR generation from the existing Python bytecode
instructions, and made an instance back end targeting the Julia Language.

The instructions passed to Julia can be described with following specification:

**P.S** : this instructions are only for Julia back end, but the process of instruction
generation will work for all prospective back ends due to an emulation of Tagless Final Style in Python:

- `from_bc.py`_
- `run_machine`_

::

    data A lhs:typing.Optional[str] rhs:'Instr';

    abc Instr;
    data App(Instr) f:Repr args:typing.List[Repr];
    data Ass(Instr) reg:Reg val:Repr;
    data Load(Instr) reg:Reg;
    data Store(Instr) reg:Reg val:Repr;
    data JmpIf(Instr) label:str cond:Repr;
    data JmpIfPush(Instr) label:str cond:Repr leave:Repr;
    data Jmp(Instr) label:str;
    data Label(Instr) label:str;
    data Peek(Instr) offset:int;
    data Return(Instr) val:Repr;
    data Push(Instr) val:Repr;
    data Pop(Instr) ;
    data PyGlob(Instr) qual:str name:str;
    data JlGlob(Instr) qual:str name:str;
    data UnwindBlock(Instr) instrs:typing.List[A];
    data PopException(Instr) must:bool;

Above code, explicitly, is for generating Python dataclasses, and for instance,

::

    abc Instr;

corresponds to

.. code-block:: Python

    class Instr(abc.ABC): pass

and,

::

    data App(Instr) f:Repr args:typing.List[Repr];

generates

.. code-block:: Python

    @dataclass
    class App(Instr):
        f:Repr
        args:typing.List[Repr]

where :code:`Repr` is short for **representation** :

::

    abc Repr;
    data Reg(Repr) n:str;
    data Const(Repr) val:object;

It's indeed a small set of instructions, and it's a mixture of stack machine instructions and
register machine instructions.

The reason why I did this is, Python bytecode instructions are
based on a stack machine and I cannot trivially eliminate the Python's :code:`Push` (e.g.,
:code:`LOAD_FAST` , :code:`LOAD_DEREF` )
and :code:`Pop` (e.g., :code:`POP_TOP` ) instructions without writing a pass to
analyze the control flows(i.e, the jump instructions by for-loops).


After generating the instructions for Julia back end, I use an existing Python-Julia bridging library to
send the those instructions to Julia, and Julia converts them to the regulr Julia objects, actually,
in the form of `algebraic data types`_:


.. code-block:: Julia

    @data Instr begin
        Value(v::Any)
        App(f:: Repr, args:: Vec{Repr})
        Ass(reg::Reg, val::Repr)
        Load(reg::Reg)
        Store(reg::Reg, val::Repr)
        JmpIf(label::Symbol, cond::Repr)
        JmpIfPush(label::Symbol, cond::Repr, leave::Repr)
        Jmp(label::Symbol)
        Label(label::Symbol)
        Peek(offset::Int)
        Return(val::Repr)
        Push(val::Repr)
        Pop()
        PyGlob(sym::Symbol)
        JlGlob(qual::Union{Nothing, Symbol}, name::Symbol)
        UnwindBlock(instrs::Vec{<:AbsA})
        PopException(must::Bool)
    end

Then codegen from the instructions.

However, Julia cannot eval the generated codes directly, otherwise will suffer from the `"World Age Problem"`_ .

To end this, I introduced my `GG.jl`_ in the JIT back end implementation, and finally the whole system works.

However, there're still performance problems.

The Python jump instructions will finally result into
some goto statements(via :code:`@goto` and `@label`) in Julia, and I said before that I haven't eliminated
push-pop instructions, which means I have to introduce a stack in the generated code. And it gets severe
when handling Python :code:`try` statements, due to the some reason I have to introduce another stack
for storing exceptions.

Above cases make it slow for Python's :code:`try` and :code:`for`. Although there're easy workarounds( :code:`monad` s instead
of :code:`try` statements, and the :code:`foreach` function instead of :code:`for` statements) to avoid
suffering the performance loss, the compile time overhead is another disaster.

So, I came up with an idea of transforming the generated instructions to fast Julia functions,
and I guess it might be a bit faster to compile than using my `GG.jl`_ .

I just posted the idea here today, though without an implementation yet.

Instead of converting generated instructions of Python objects to Julia instances,
we can also translate them to Julia types, and then we write an interpreter-like
function in Julia to interpret the types, and the fast performance of Julia's
generated functions may help this case, and even do better to eliminate my stack
encoding. Of course, it's just a guess now. I tried to finish it today, but it seems
still need more time.

I'll use a pure stack machine instructions to show the design:

1. make some types for representing instructions

.. code-block:: Julia

    abstract type S end
    struct S0 <: S end
    struct S1{T} <: S end
    struct S2{T, G} <: S end

    struct App{N} end
    struct Push{Val} end
    struct Pop end
    struct Ass{Sym} end
    struct Label{L} end
    struct Jmp{L} end
    struct JmpIf{L} end
    struct Return end

2. based on the types, I write an interpreter-like generated function:


.. code-block:: Julia

    function interp(_, ::Type{S2{Return, Tail}}, __stack__::Tuple{Hd, Tl}, _, _)::Hd where {Tail, Hd, Tl}
        stack[1]
    end

    pop_n(x, n) = n === 0 ? (Expr[], x) :
        begin vs, tl = pop_n(:($x[1]), n-1)
            push!(vs, :($x[0]))
            vs, tl
        end

    @generated function interp(
        past_instrs::Type{Past},
        left_instrs::Type{S2{App{N}, Follow}},
        __stack__,
        ::Type{S1{LocalNames}},
        localvars...
    ) where {Past, N, LocalNames, Follow}
        n = hd.parameters[1]
        elts, tl = pop_n(:__stack__, N)
        f = elts[1]
        args = view(elts, 2:n+1)

        quote
            __stack__ = ($f($(args...)), $tl)
            interp(S2{App{N}, Past}, Follow, __stack__, S1{Names}, localvars...)
        end
    end


Some explanations:

- The argument :code:`past_instrs` for prospective jumping back. To implement loops, jumping back is needed.
- The argument :code:`localvars` is all local variables could be accessed in the function.
- The type parameter :code:`LocalNames` is for statically looking up local variables from its names.

I'm to implement it in the future, to examine if this way will keep performance and reduce the JIT overhead.

If not, the Julia back end for the Restrain Python JIT might be useless(unless for computations in really big scales).

If Julia cannot be a good JIT back end for CPython, I'll try JVM. JVM is a stack machine and it's not that slow to
boot up. Further, the .class files can be loaded dynamically and also with JIT support, and my friends who know
much about Java and JVM told me JVM is also good for optimizing the dynamically typed codes.

However, if I do use JVM as a back end in the future, I think keeping CPython's C extensions working will be difficult.

Actually I'm not sure, because I didn't know much about JVM, but if it's the truth, I'll give up trying JVM.

Keeping `CPython Compatible`_ is the most important, in my opinion.


.. _from_bc.py: https://github.com/thautwarm/restrain-jit/blob/8c158693e4548da799bdcab2858ca2b64e1f0521/restrain_jit/abs_compiler/from_bc.py
.. _run_machine: https://github.com/thautwarm/restrain-jit/blob/8c158693e4548da799bdcab2858ca2b64e1f0521/restrain_jit/vm/am.py#L267
.. _Restrain Python JIT: https://github.com/thautwarm/restrain-jit/
.. _algebraic data types: https://github.com/thautwarm/RestrainJIT.jl/blob/cf7c1d7c0b6eb517db46ae1b2339b560e2b0a2a1/src/instr_repr.jl
.. _"World Age Problem": https://discourse.julialang.org/t/world-age-problem-explanation/9714
.. _GG.jl: https://github.com/thautwarm/GeneralizedGenerated.jl
.. _CPython Compatible: https://github.com/thautwarm/restrain-jit/blob/master/docs/What-is-CPython-Compatible.md