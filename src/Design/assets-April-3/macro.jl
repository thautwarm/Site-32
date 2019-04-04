using MLStyle

function setup_dbg(exprs, line, debug_mod)
    quote
        $line
        try
            $(exprs...)
        catch __err__
            $debug_mod.eval(quote
                $([:($k = $(QuoteNode(v)))  for (k, v) in (Base.@locals)]...)
            end)
            __line__ = $line
            @info :line_number __line__
            @info :error __err__
            @info :locals Base.@locals
            while (q = readline(); lowercase(strip(q)) != "q")
                try
                    ans = $debug_mod.eval($Meta.parse(q))
                    @info :ans ans
                    $debug_mod.eval(:(ans = $ans))
                catch __err__
                    @info :error __err__
                end
            end
        end
    end
end

function debug(expr, line, debug_mod)
    @match expr begin
        Expr(:block, elts...) => begin
            lastline = line
            new_elts = []
            segment = []
            for (i, each) in enumerate(elts)
                @match each begin
                    l::LineNumberNode =>
                        begin
                        if !isempty(segment)
                            push!(new_elts, setup_dbg(segment, lastline, debug_mod))
                            segment = []
                        end
                        lastline = l
                        end
                    a =>
                    begin
                        push!(segment, debug(a, lastline, debug_mod))
                    end
                end
            end
            if !isempty(segment)
                push!(new_elts, setup_dbg(segment, lastline, debug_mod))
            end
            Expr(:block, new_elts...)
        end
        Expr(head, elts...) =>
            let lastline = line
                map(elts) do each
                    if each isa LineNumberNode
                        lastline = each
                    end
                debug(each, lastline, debug_mod)
                end |> elts -> Expr(head, elts...)
            end
        a => a
    end
end

macro debug(expr, debug_mod)
    ex = debug(expr, __source__, eval(debug_mod))
    esc(ex)
end


module Debugger
end

@debug function f(z)
        !z
end Debugger

f(1)
