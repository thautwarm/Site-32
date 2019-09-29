===========================================
Thautwarm's Open Source Contributions
===========================================

This is a collection of thautwarm(Taine Zhao/Wanghongxuan Zhao)'s open source contributions,
but might not be that new.

Projects Owned By Other Individuals/Organizations
------------------------------------------------------

- `Python/cpython <https://github.com/python/cpython>`_
    - better representation for :code:`mmap` : https://github.com/python/cpython/pull/9891

- `Python/typeshed <https://github.com/python/typeshed>`_
    - correct typing for the AST :code:`ImportFrom` : https://github.com/python/typeshed/pull/2517

- `vstinner/bytecode <https://github.com/vstinner/bytecode>`_
    - fix calculating the stack effects: https://github.com/vstinner/bytecode/pull/43
    - support more wide kinds of constants for :code:`LOAD_CONST` instruction : https://github.com/vstinner/bytecode/pull/36

- `JuliaLang/Julia <https://github.com/JuliaLang/julia>`_
    - good at detecting bugs: https://github.com/JuliaLang/julia/issues/created_by/thautwarm

- `JuliaCN/Py2Jl.jl <https://github.com/JuliaCN/Py2Jl.jl>`_
    - as a maintainer. The Python to Julia transpiler.

- `JuliaCN/MeetUpMaterials <https://github.com/JuliaCN/MeetUpMaterials>`_
    - lectures of my talk in the 2019 meetup: https://github.com/JuliaCN/MeetUpMaterials/pull/20

- Microsoft/GraphEngine
    - as a previous member and contributed a lot: https://github.com/Microsoft/GraphEngine/pulls/thautwarm

- cscherrer/Soss.jl
    -  as a collaborator: https://github.com/cscherrer/Soss.jl

- MasonProtter/ReplMaker.jl
    - add an API for switching REPL modes: https://github.com/MasonProtter/ReplMaker.jl/pull/6

- YingboMa/AsmMacro.jl
    - refactor codebase through pattern matching: https://github.com/YingboMa/AsmMacro.jl/pull/4

- `QuantumBFS/YaoQASM.jl <https://github.com/QuantumBFS/YaoQASM.jl>`_ 
    - as a maintainer. A parser of QASM and a QASM compiler targeting QBIR.

- f0rki/mapping-high-level-constructs-to-llvm-ir
    - documentations: https://github.com/f0rki/mapping-high-level-constructs-to-llvm-ir/pull/19

- ustclug/mirrorhelp
    - improve Haskell help docs: https://github.com/ustclug/mirrorhelp/pull/130

- rocky/python-uncompyle6
    - find bugs: https://github.com/rocky/python-uncompyle6/issues/created_by/thautwarm

- alexandrebarachant/muse-lsl
    - fix unrunnable codes of README: https://github.com/alexandrebarachant/muse-lsl/pull/85

Personal Projects Used Widely
------------------------------------------------------


- GeneralizedGenerated.jl: A library greatly enhances staged programming of Julia
    - https://github.com/thautwarm/GeneralizedGenerated.jl

- MLStyle.jl: A library provides advanced functional programming infrastructures(like, extensible pattern matching) for Julia
    - https://github.com/thautwarm/MLStyle.jl

- graphviz-artist: A library provides a higher level APIs for Graphviz in Python
    -  https://github.com/thautwarm/graphviz-artist


Other Personal Projects With Great Personal Efforts Prospective Values
------------------------------------------------------------------------------------------------------------

- `RBNF.hs <https://github.com/thautwarm/RBNF.hs>`_
    A language-independent parser generator.

    Actually an advance based on the LL(k) parsing, but equipped with several extensions like

    - left recursion resolutions(yes, for LL parsing),

    - grammar inlining(without causing reduction conflicts),

    - more advanced syntax-driven features,

    - context-sensitive stuffs like predicates according to global/local contexts,

    - lookahead optimizations based on decision trees.

    Additionally, the time complexity can regress to linear without using context-sensitive stuffs.

- `FSTan  <https://github.com/thautwarm/FSTan>`_
    A F# library providing a more usable light-weighted higher-kinded types.

- `RSolve.hs <https://github.com/thautwarm/RSolve>`_
    A Haskell library providing easy-to-use monadic interfaces for constraint solvers, bundled with examples like HM unifications.

- `RSolve.py <https://github.com/thautwarm/rsolve.py>`_
    A Python library providing easy-to-use and intuitive OOP interfaces for constraint solvers, bundled with examples like HM unifications.

- `Idris-Cam <https://github.com/thautwarm/idris-cam>`_
    An abstract codegen backend for Idris programming language, bundled with backend implementations in Python and Julia.

    The Python implementation: https://github.com/thautwarm/idris-python.

- `restrain-jit <https://github.com/thautwarm/restrain-jit>`_
    A more available JIT compilation for CPython.

- `moshmosh <https://github.com/thautwarm/moshmosh>`_
    The Python syntax extensions, bundled with the fastest implementation of pattern matching in Python world.

    According to the benchmark, the pattern matching is at least 4 times faster than that from the Pampy project.

    Besides, unlike Pampy, moshmosh won't introduce lots of APIs or lead to a steep learning curve.

- `do-you-like-wan-you-si <https://github.com/thautwarm/do-you-like-wan-you-si>`_
    Automatically playing Fate/Grand Order.

- `CanonicalTraits.jl <https://github.com/thautwarm/CanonicalTraits.jl>`_
    Real traits/typeclasses implemented in Julia. With support of functional dependencies and default implementations.

- `HigherKindedPolymorphisms.jl <https://github.com/thautwarm/HigherKindedPolymorphisms.jl>`_
    Higher kinded types for Julia(light-weighted higher-kinded polymorphisms).


For more open source contributions, check my GitHub profile.
