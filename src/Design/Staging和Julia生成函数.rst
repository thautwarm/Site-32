Staging和Julia生成函数
========================

Staging是什么
-------------------------

有一种叫做  **staging** 的技术, 它通过将程序运行时分割成更多的编译/运行期,
每一阶段, 都会运用相比之前更加丰富的信息, 去生成更加高效的程序.

而在静态编译的情况下, 若非人为操作编译器的internal API,
所有的Staging优化都不可能完成, 因此带着native code编译器的动态语言,
例如Julia, 很多时候很容易在性能上远胜静态语言.

一个经典的案例来源于 `Oleg Kiselyov的这篇"博客" <http://okmij.org/ftp/meta-programming/calculi.html>`_ :

给定 :math:`Fibonacci` 数列的

* 前两项 :math:`a_1, a_2` ,
* 以及递推关系的运算 :math:`f` , 其中 :math:`f(a_{k}, a_{k+1}) = a_{k+2}`

求解第 :math:`n` 项.

问题解的直观定义如下

.. code-block:: julia

    function fibnr(plus::Plus, x, y, n::Int) where Plus <: Function
        n == 0 && return x
        n == 1 && return y
        plus(fibnr(plus, x, y, n-1), fibnr(plus, x, y, n-2))
    end

    [fibnr(+, 0, 1, i) for i in 1:10]
    out:
        10-element Array{Int64,1}:
        1
        1
        2
        3
        5
        8
        13
        21
        34
        55

    using BenchmarkTools

    @btime fibnr(+, 0, 1, 20)
    out:
        42.766 μs (0 allocations: 0 bytes)

注意 ``1 us = 1000 ns`` .

而staging技术将程序的运行分为多个阶段,
搜集前一阶段信息, 去优化后一阶段要执行的代码.

在上述案例里, 当我们的问题是 **计算给定** :math:`n` **后不同的** :math:`f` **和** :math:`a_1, a_2` 时,
一个直接的staging实现如下:

.. code-block:: julia

    islinenumbernode(x) = x isa LineNumberNode
    rmlines(ex::Expr) = begin
            hd = ex.head
            tl = map(rmlines, filter(!islinenumbernode, ex.args))
            Expr(hd, tl...)
        end
    rmlines(a) = a

    fibnr_with_n(n::Int64) = begin
            splus(x, y) = :($x + $y)
            quote
                function (f :: Plus, x::Int64, y::Int64) where Plus <: Function
                    (+) = f
                    $(fibnr(splus, :x, :y, n))
                end
            end |> rmlines
        end

我们使用一下这个代码

.. code-block:: julia

    fibnr_with_n(1)
    out:
        quote
            function (f::Plus, x::Int64, y::Int64) where Plus <: Function
                (+) = f
                y
            end
        end

    fibnr_with_n(2)
    out:
        quote
            function (f::Plus, x::Int64, y::Int64) where Plus <: Function
                (+) = f
                y + x
            end
        end

    fibnr_with_n(5)
    out:
        quote
            function (f::Plus, x::Int64, y::Int64) where Plus <: Function
                (+) = f
                (((y + x) + y) + (y + x)) + ((y + x) + y)
            end
        end

    fib_20 = eval(fibnr_with_n(20))
    @btime fib_20(+, 0, 1)
        17.538 ns (1 allocation: 16 bytes)

可以看到, 对 ``n=20`` , 也就快了1000多倍, 并且这个性能差距是随规模而增大的.

观察 ``fibnr_with_n(5)`` 生成的代码, 可以发现, 有很多项被重复地计算了,
例如 ``(y+x)`` 和 ``((y+x) + y)``.

我们可以使用 **编译期记忆化技术** 来继续优化代码.

.. code-block:: julia

    genlet!(expr, memo::Vector{Pair{Any, Symbol}}, counter::Ref{Int}) = begin
            number = counter.x
            counter.x += 1
            sym = Symbol(:x, number)
            push!(memo, expr=>sym)
            sym
        end

    fibnr_with_n_and_memo(n::Int64) = begin
            memo = Vector{Pair{Any, Symbol}}()
            counter = Ref(0)
            splus(x, y) =
                let computation = :($x + $y)
                    computed_where = findfirst(memo) do (k, v)
                        k == computation
                    end
                    computed_where != nothing && return memo[computed_where].second
                    genlet!(computation, memo, counter)
                end
            body = fibnr(splus, :x, :y, n)
            letbindings = [:($v = $k) for (k, v) in memo]

            quote
                function (f::Plus, x::Int64, y::Int64) where Plus <: Function
                    (+) = f
                    $(letbindings...)
                    $body
                end
            end |> rmlines
        end

    fibnr_with_n_and_memo(5)
    out:
        quote
            function (f::Plus, x::Int64, y::Int64) where Plus <: Function
                (+) = f
                x0 = y + x
                x1 = x0 + y
                x2 = x1 + x0
                x3 = x2 + x1
                x3
            end
        end

    fib_20_with_memo = eval(fibnr_with_n_and_memo(20))
    @btime fib_20_with_memo(+, 0, 1)
    out:
        18.128 ns (1 allocation: 16 bytes)

看起来似乎还变慢了? 但实际情况是, 和未优化重复项时没有差别, 因为Benchmark的指示量有浮动.

为什么没有变快, 这是因为Julia的编译器优化能力很强, 避免了重复项的计算.

但这不意味着使用编译期记忆化就毫无意义, 对于简单的数字运算, 优化重复项很简单,
但运算规模大了之后就会给编译器带来很大的负担.

除开不依赖编译器的性能优化外, 编译期记忆化还使得生成的代极为简洁:

.. code-block:: julia

    fibnr_with_n_and_memo(10)
    out:
        quote
            function (f::Plus, x::Int64, y::Int64) where Plus <: Function
                (+) = f
                x0 = y + x
                x1 = x0 + y
                x2 = x1 + x0
                x3 = x2 + x1
                x4 = x3 + x2
                x5 = x4 + x3
                x6 = x5 + x4
                x7 = x6 + x5
                x8 = x7 + x6
                x8
            end
        end

    fibnr_with_n(10)
    out:
        quote
            function (f::Plus, x::Int64, y::Int64) where Plus <: Function
                (+) = f
                ((((((((y + x) + y) + (y + x)) + ((y + x) + y)) + (((y + x) ...
            end
        end

``fibnr_with_n(10)`` 生成的代码, 有一行长达 **531个字符** 的表达式计算;
而 ``fibnr_with_n_and_memo(10)`` 的结果则一目了然.


运行时函数构造: Julia生成函数
------------------------------------------------------------


"世界纪元"("World Age")问题
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

前面我们介绍了staging, 作为一个优化利器, 这很美好.

但在Julia中, ``eval`` 只能在模块顶层(全局作用域)创建函数, 否则,
将会导致一种名为 `"世界纪元" <https://discourse.julialang.org/t/world-age-problem-explanation/9714>`_ 的问题.


(注: "Wolrd Age"翻译为"世界纪元"听起来极为酷炫, 所以我必定要在这里使用一次. 但究其语义, "编译期阶数"之类的或许更为合适)

"世界纪元"问题, 反映为一个模块在运行时通过 ``eval`` 等手段创建的函数,
无法在直接在该模块内部引用.

如果允许在模块随时随地定义新函数, 并在模块内部调用它,
那么, 模块中已经编译优化的对象, 都可能发生改变(例如被重定义等).

实际上, 这种过于灵活的行为使得一切都有可能发生, 同时,
几乎每一种编译器的优化, 都需要某种条件的成立, 也就是代码满足确定的约束.

为了优化, 随时随地定义函数的功能受到了一定的限制, 表现为"世界纪元"问题.

只有在模块顶层/全局作用域, 用 ``eval`` 等手段创建的函数才能在该模块内部被直接引用.

我们看一个案例

.. code-block:: julia

    module Mod

    """
    make functions dynamically
    """
    function make_func_dyn(exp)
        eval(exp)
    end


    add2(x) = make_func_dyn(:(x -> x + 2))(x)

    end

    # Case1:
    f1 = Mod.make_func_dyn(:(x -> x + 2))
    @info :case1 f1(10)

    # Case2:
    @info :case2 Mod.add2(10)

上述代码中, ``case1`` 的代码正常执行, 而 ``case2`` 则抛出了错误:

::

    ┌ Info: case1
    └   f1(10) = 12

    ┌ Error: Exception while generating log record in module Main at case.jl
    │   exception =
    │    MethodError: no method matching (::getfield(Main.Mod, Symbol("##3#4")))(::Int64)
    │    The applicable method may be too new: running in world age 25581, while current world is 25582.


"世界纪元"问题可以用 `invokelatest <https://discourse.julialang.org/t/world-age-problem-explanation/9714/4?u=thautwarm>`_ 解决,

但 `代价是失去性能, 永远和快沾不上边 <https://docs.julialang.org/en/v1/base/base/index.html#Base.invokelatest>`_ :

            (The drawback is that invokelatest is somewhat slower than calling f directly, and the type of the result cannot be inferred by the compiler.)

现在我们回到之前 :math:`Fibonacci` 数列的例子.


从Fibonacci数列到Julia生成函数
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

一. 首先我们说明, 这里存在"世界纪元"问题.

试想, 当要固定的 :math:`n` 不是一个可以静态地(在编译时期, 在模块顶层)确定的数,
而需要和外部环境商量才能得到, 例如, 一个文件的行数, JSON数据中某种对象的数量. 此时,
必然需要 **在运行时创建staged函数** .

二. 那么, 怎么解决"世界纪元"问题呢?

Staging主要目的就是优化加速, 要是使用 ``invokelatest`` , 就失去了意义.

此时, 我们就得介绍今天的主角, Julia强无敌的 `生成函数(generated function) <https://docs.julialang.org/en/v1/manual/metaprogramming/index.html#Generated-functions-1>`_ .

Julia生成函数, 其功能上是运行时 ``eval`` 的一个子集.

它的参数的定义, 和普通函数一模一样, 但特别是, **它的函数体分为两个阶段** .

下面是一个生成函数 ``assoc_ap`` 的定义.

.. code-block:: julia

    """
    一个满足结合律的运算符
    """
    @generated function assoc_ap(x :: T, y :: T) where T
        @assert x == T
        @assert y == T
        if x <: Number
            :(x + y)
        elseif x <: AbstractString
            :(x * y)
        else
            throw(MethodError(assoc_ap, T, T))
        end
    end

    @assert assoc_ap(1, 2) == 3
    @assert assoc_ap("2", "33") == "233"

对于上面的代码, 注意两点:

1. ``assoc_ap`` 返回Julia的语法树
2.  在返回值的语法树之外, ``assoc_ap`` 的参数 ``x`` , ``y`` 实际上 **代表相应参数的类型**

生成函数的函数体, 其返回值(一棵语法树)以外的地方, 是第一个阶段, 是小的编译期, **它可以利用参数的类型信息** ,
生成一份代码(一棵语法树), 该代码则是第二个阶段, 真正执行函数调用的阶段.

Racket的粉丝如果粗略看一眼, 会觉得啊, 这有什么了不起. 这是我遇到的实际情况.

但Julia的生成函数是无开销的, 事实上, 因为某些已知的原因, 还经常比静态定义的函数更快.

定义Julia生成函数, 本质上是定义了一个生成器, 而受益于Julia的多重分派, 静态类型推导以及一些优化,

**这个生成器在无需用户关心的情况下, 对每一组不同分派下的参数类型做且仅做一次生成** , 并且,
在不使用危险的动态特性时, **可以inline** , **可以静态分派** .

::

 @btime assoc_ap(1, 2)
 out:
  0.017 ns (0 allocations: 0 bytes)

这个速度, 对于有经验的Julian来说, 一眼就知道它全身上下, 都被静态实现查找, 内联和常量化得彻彻底底.

利用生成函数, 我们轻松地改写 :math:`Fibonacci`  数列的staging实现.

.. code-block:: julia

    @noinline fibnr_staged_n(f, x, y, n) = fibnr_staged_n(f, x, y, Val(n))

    @noinline @generated function fibnr_staged_n(f :: Plus, x, y, ::Val{n}) where {n, Plus <: Function}
        memo = Vector{Pair{Any, Symbol}}()
        counter = Ref(0)
        splus(x, y) =
            let computation = :($x + $y)
                computed_where = findfirst(memo) do (k, v)
                    k == computation
                end
                computed_where != nothing && return memo[computed_where].second
                genlet!(computation, memo, counter)
            end
        body = fibnr(splus, :x, :y, n)
        letbindings = [:($v = $k) for (k, v) in memo]
        quote
            (+) = f
            $(letbindings...)
            $body
        end |> rmlines
    end


使用方法很简单

.. code-block:: julia

    fibnr_staged_n(+, 0, 1, 10)
    out: 55

    fibnr_staged_n(+, 0, 1, 9)
    out: 34

    @btime fibnr_staged_n(+, 0, 1, 20)
    out: 0.017 ns (0 allocations: 0 bytes)

虽然看似性能提升很更多, 但通过 ``@code_llvm`` 可以看到 ``fibnr_staged_n(+, 0, 1, Val(20))`` 和
``eval(fibnr_with_n_and_memo(20))(+, 0, 1)`` 的LLVM IR是完全一致的.

性能差距的原因, 个人见解是, 类似C++开O3后不易benchmark, 因为编译器优化的能力太强, 做了太多内联和常量化.

Julia生成函数的优越性
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

我们再提一次之前讲过一句话:

**在无需用户关心的情况下, 对每一组不同分派下的参数类型做且仅做一次生成**.

Julia的生成函数, 是一种兼顾简便, 编程效率和性能的staging实现.

除开其追求极致的性能外, 就简便和编程效率我们做如下两点分析.

一. Julia生成函数拥有着和普通函数一样的使用方法, 这意味着,
使用生成函数的Julia库即便引入大量的staging优化,
用户也不需要承受任何相关的心智负担.

二. Julia生成函数是自动生成函数. 受益于Julia的多重分派,
用户在使用staging前,
**不需要** 分析出 **第一次使用某一组类型参数对应的程序** 的位置,
更不需要进行对生成的代码进行手动记忆化来避免再次生成程序.

具体到 :math:`Fibonacci` 数列的例子,
如果没有Julia这样的生成函数, 那么对于任意给定的 :math:`n` ,
我们必须分析函数在哪些位置可能被第一次被调用(常常会是所有调用点),
并在这些位置检查是否已经代码生成, 还可能需要维护用于记忆化的表的结构.

这些都可能导致性能问题, 却都是Julia在静态类型推导和JIT优化下帮我们搞定的事.