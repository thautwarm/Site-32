# SMLFS

SMLFS: standard MLFS.
MLFS: A type system: namely, Raising **ML** to the power of system **F** in a **S**implest way

???所以这是什么？

我们知道虽然有JavaScript，但很多人却倾向于使用TypeScript。

**SMLFS之于Julia, 就是TypeScript之于JavaScript**。

## Julia动态类型的问题

**Julia的类型是为性能优化而设计的**, 这意味着对于使用者而言并不一定很方便。

```julia
function int_add1_or_zero(x)
    if x isa Int
        x + 1
    else
        0
    end
end
```

这个代码在调用时，如果参数是整数，

Julia把它优化到

```julia
int_add1_or_zero(x::Int) = x + 1
```

否则，同样能优化
```julia
int_add1_or_zero(_::Any) = 0
```

在Julia中，`isa`操作很少真正进行，因为通常只需要少数的isa，就可以确定后续代码中的变量类型，从而调用完全静态特化的代码。

可是，对于函数的参数类型`T`和返回类型`R`, 如果`T`和`R`之间的关系非常复杂，乃至以来运行时的结果，这很多时候不是给Julia这种super compiler增加压力，优化都没问题————

有问题的是代码的可读性和人的理解能力。

在这个意义上，有一类叫做ML的语言大家族，他们依靠很多强大的**静态类型系统**,
能够表达出简洁而富有表现力的类型，很多时候并不带来性能优化，但能够帮助程序员检查错误，以及智能提示。

而MLFS是这些类型中最为强大的之一。

另外，Julia动态类型，带来的问题不仅是不好读，还会导致一些抽象的问题。

标准库中有函数

```julia
Base.map(f, xs) = ...
Base.map(x -> x + 1, [1, 2]) == [2, 3]
```


```julia
abstract type HM end
struct H <: HM end
struct M <: HM end
```

假如我们需要某个处理一串`HM`的函数(实际上这个写法有问题)

```julia
function f(::AbstractVector{HM})
    ...
end
```

那么问题来了，
现在我有一个

```julia-console
hms = [H(), M()]

2-element Array{HM,1}:
 H()
 M()
```

那么假如我们有个获取数据的函数

```julia
function get_hms_from_ints(xs::AbstractVector{Int})
    [x > 0 ? H() : M() for x in xs]
end

f(get_hms_from_ints([1, 2, -1, -2]))
```

到目前为止都很好, 但

```julia-console
julia> f(get_hms_from_ints([1, 2, 3, 2]))
ERROR: MethodError: no method matching ff(::Array{H,1})
Closest candidates are:
  ff(::AbstractArray{HM,1}) at REPL
```

这也就是为什么，常常你需要把参数写成协变容器类型:

```julia
function f(::AbstractVector{E}) where E <: HM
    ...
end
```

然后真实世界有很多的不同的需求，有时候需要逆变(如果`Vector{HM}`是没有必要的，那为什么又会支持它？)。

在数据处理中还会出现`Vector{Vector{T} where T}`这样的类型...

而Julia的`[a]`, `[]`, `[a for a in ...]`，在这种情况下，很难隐式推导出符合输入要求的类型。

所以说，让我们看SMLFS，让Julia走向静态的语言。

## Standard MLFS

SMLFS实际上没有语法，它其实只是个编译器。

目前我们有两个语法前端，一个利用Julia的parser实现, 看似Julia, 但语义有很多地方不相同。

另一个则是用Python实现的。

两个前端各有各的好处。

```mlfs
module main
import Prim
open Prim # 或者 using Prim, 引入Prim模块的名字

let vec = Vec ()
let _ = vec.push(1)

check vec as vec_type
```

我们得到输出

```julia
vec_type : Vec i64
```

可以发现类型可以被隐式推导。


而以下程序则会在编译期报错:

```mlfs
module main
import Prim
using Prim

let vec = Vec ()
let _ = vec.push(1)
let _ = vec.push("1")
```

julia的前端(目前break掉了，所以无法运行成功)

```julia
module main

using Prim
vec = make Vec
vec.push(1)
vec.push("2")
vec.?(vec_type)
```

但在原生的Julia中,

```julia
module Main

vec = []
push!(vec, 1)
push!(vec, "2")
```

`vec`的类型是`Vector{Any}`.


## SMLFS的静态特性

1. Higher Rank Types
2. Higher Kinded Types
3. Scoped type variables

...
