# Research: Review and Observations of Python JIT

When we talk about Python, a popular industrial programming language,
it is well-known that Python's Just-In-Time(JIT) compilation support has become a long-term issue.

A JIT compiler which will fundamentally speed up the program execution is likely to make
a big impact. Recently, it seems that the majority of dynamic programming languages have already supported JIT,
[even for Ruby](https://www.ruby-lang.org/en/news/2018/12/25/ruby-2-6-0-released/), which is usually compared with Python. Further, Python is still performing an important role in some important domains of web backend, machine learning and so on. In these domains, as making heavy and nested abstractions in Python is costly, performance disasters arise, such as whenever you need a Python loop over a collection whose size is proportional to the problem.

Figuring out an approach to make an easily-available and fully compatible(highlight: **C extensions + work for ordinary CPython**) Python JIT, is no doubt a research which is not only technically impactful, but also economically valuable. 

I for one, have spent quite a few years investigating and experiencing on the topics related to Python JIT. 
To conduct this research of Python JIT, it is considered beneficial for us to review the history of Python JIT,
and propose our initial observations distinguished from the previous attempts.

## History

There're several attempts on Python JIT, and the major are [Pyston](https://github.com/dropbox/pyston), [Pyjion](https://github.com/microsoft/Pyjion), [PyPy](https://bitbucket.org/pypy/pypy/src) and [Numba](http://numba.pydata.org/). Still are there other respectable attempts like Psyco, JitPy or HOPE etc., about which we heard less.

In a general point of view, PyPy shall be the most successful among all existing Python JITs. In the early years it used **partial evaluation(PE)**, and finally turned to **meta tracing**. They gave the reason [here](https://morepypy.blogspot.com/2018/09/the-first-15-years-of-pypy.html#why-did-we-abandon-partial-evaluation), which needs our attention and study, but according to different goals and technique standpoints/preferences between us and PyPy team, it's too early to say PE is worse than tracing. To be honest, I do have lots of opinions in their final conclusions and decisions.


Pyjion presented 3 goals in its [GitHub Homepage](https://github.com/microsoft/Pyjion#what-are-the-goals-of-this-project). The design looks good, even just the first one. I'm really curious about whether their first goal is achieved or not, which is about making Python JIT customizable. Pyjion are mainly based on the [Microsoft .NET Roslyn Compiler](https://github.com/dotnet/roslyn), and leads to some advantages like mature techniques and avoiding implementing JIT optimization algorithms, but some downsides like heavy dependencies and heavily referencing external frameworks as well.

Additionally, Pyjion didn't announce a drop, while their development is quite inactive. As a result, the pronunciation of Pyjion, "pigeon", just reminds me of a Chinese meme "咕咕". This Chinese meme is for describing a person who's always postponing his work but telling "Yes yes, I'm working". No offense but just for fun.

Pyston is also a pretty successful one, but dead for lack of money. Although they finally gave up developing Pyston and chose Go programming language for the initial purpose then, [they did make a progress](https://blog.pyston.org/). Their technique seems called inline caches(ICs). This looks very similar to the old Psyco JIT compiler and what I did in [Restrain-JIT Cython backend](https://github.com/thautwarm/restrain-jit/tree/master/restrain_jit). I'm in favour of it haha, but I don't think it costly in time and money. They should employ me at that time. What's more, Pyston improves runtime with LLVM, which I'm also kind of familiar with.

Numba seems to be focusing on numeric computing, but I know they did a lot to support JITing generic programs. I think so far(2020.04) Numba can compile all Python programs into the LLVM versions with its `object mode`, but it is still a mystery for me that why calling a Numba compiled function has so huge overhead. This fact just gets observed when the compiled function is really small, like just an addition(`def f(a, b): return a + b`), you amazingly find a 100x performance hit. Cython has no such issues. Anyway Numba can be very fast due to its type inference and LLVM codegen, but usually difficult to find use cases other than its `nopython mode` works, i.e., numeric computing.

There're also things like GraalPython, which is also not a fully compatible Python JIT because you cannot use it in CPython, the dominant Python implementation that Python developers are working with.

The obstacles shown to the previous Python JIT attempts, just suggest the inactivity of the motion of Python community towards sacrificing some compatibility: The widely used(generally available) Python JIT are Numba and PyPy, where the former drops making a big news for full-featured Python, while the latter just makes a new runtime and somewhat a programming language. All attempts to achieving general purpose and full compatibility, just failed.


## Observations

I feel like to mention Psyco here, one of the earliest attempts. 

Armin Rigo tried to specialize the function calls with arguments' Python types during the runtime, [which](http://psyco.sourceforge.net/psyco.ps.gz) is really a concise and impressive speed up strategy and gives me a familiarity of some kind of PE, Pyston's IC, or part of Julia programming language's JIT.

What if we want to achieve some **stably-effected, easily-compatible, concisely-implemented, not-that-aggressive and extensible** JIT?

I'd appreciate Psyco's efforts at that early stage.

Also, encounter with a programming language called Julia inspired me a lot. Julia shows how powerful it could be to simultaneously apply staging(a PE technology), runtime type inference, runtime specialization and many other features. It shall be nice for me to adopt some techniques from Julia for the Python JIT research.

Julia and Psyco are like existing proof-of-concepts, which makes me strongly believe the brightness of the future of this research. 

Moreover, I've already made progress by developing the project called Restrain-JIT, which I've mentioned afore in this document. In Restrain-JIT, I use Cython, a fastest Python to C/C++ compiler, to compile runtime generated code, and rewrite Python bytecode to support monitoring "hot code", perform control flow analysis and Relooper-like conversions to allow Python stack virtual machine to Cython source code with simple register allocation optimizations, and finally got a stable speed up in a subset of Python.

This attempt gave me a chance to present a talk in PyConChina 2019, and the corresponding slides(barely in Chinese but the figures are easy to understand) are available at [this link](https://github.com/PyConChina/2019-Slides/blob/master/Chengdu/3_Thautwarm_%E8%A7%A3%E6%94%BEpython%E7%9A%84%E8%A1%A8%E8%BE%BE%E5%8A%9B_%E6%80%A7%E8%83%BD%E5%92%8C%E5%AE%89%E5%85%A8%E6%80%A7_%E8%AF%AD%E6%B3%95%E5%92%8C%E8%AF%AD%E4%B9%89%E6%89%A9%E5%B1%95_JIT_%E9%9D%99%E6%80%81%E6%A3%80%E6%9F%A5.pdf) where the JIT part starts from page 24.

(TODO)