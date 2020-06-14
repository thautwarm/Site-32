# Julia Benefits from Tagless Final

It's an article written extremely casually, and very short.

## What Happened Today?

Today I found someone in Julia discourse asked me some [questions about ADT encoding in Julia](https://discourse.julialang.org/t/are-there-idioms-in-julia-for-fast-algebraic-data-types-adt/37244/20).

About the performance issues with Julia structures and subtyping for ADT emulations, I said it's impossible to address.

Yes, it's impossible for Julia to have fast ADTs, so far.

However, we have an alternative, to achieve everything that ADTs can achieve, in Julia.

It's called tagless final.

There're many resources about tagless final: [http://okmij.org/ftp/tagless-final](http://okmij.org/ftp/tagless-final) .

As Julia cannot encode ADTs/GADTs perfectly, with tagless final, the problem just gets eased.


## An Example Turns Out to be Type Unstable

```julia
abstract type Expression end

struct Const <: Expression
    value :: Int
end

struct Add <: Expression
    lhs :: Expression
    rhs :: Expression
end

evaluate(e::Const) = e.value
evaluate(e::Add) = evaluate(e.lhs) + evaluate(e.rhs)

@code_warntype evaluate(Add(Const(2), Const(3)))
```

Input above code in Julia shell, you got "a big piece of red", which means type unstable(it's just what `@code_warntype` does).


Above code is an emulation of ADTs, and ADT approach is called initial approach in the research of tagless final.

We can have the final approach, which turns out to be superior in Julia.

## Tagless Final Encoding about Above Code


```julia
struct SYM{F1, F2}
    constant :: F1
    add :: F2
end

function constant(v)
    function (sym::SYM)
        sym.constant(v)
    end
end

function add(term1, term2)
    function (sym::SYM)
        sym.add(term1(sym), term2(sym))
    end
end

# self algebra
self = SYM(constant, add)

evaluate =
    let constant(v::Int) = v,
        add(l::Int, r::Int) = l + r
        SYM(constant, add)
    end


@code_warntype add(constant(2), constant(3))(evaluate)
```

All in blue, all in blue, type stable all in blue!

How could you get the full mechanisms of turning ADTs to tagless final?

If you understand Haskell, you shall read [First-class Pattern Matching in the Final Approach](tagless-final-pattern-match.md).

Otherwise, learn ocaml and go to Oleg-sensei's website.
