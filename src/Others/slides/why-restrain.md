## Motivation of developing a new Python JIT

Currently, the 2 "successful" Python JIT are PyPy and Numba.

However,


### What is Absent in PyPy


1. PyPy is a new Python, running in a separate runtime, hence
   PyPy cannot easily get used by the **CPython**, which is the
   dominant Python distribution and implementation.
   
   **When we're talking about Python, we're very likely taking about CPython**.

2. C Extensions for PyPy is slow, as it has to **imitate** Python's **C APIs**,
   and **memory layouts**.



The first item is the reason why CPython people cannot take advantage of PyPy.

The second item is the reason why PyPy cannot alter CPython.


###  What is Absent in Numba

0. Numba is applicable to CPython world.
1. For numeric computing, Numba uses its `nopython mode` to be very fast.

2. For general-purpose programming, Numba can compile Python bytecode instructions to LLVM IR and further, native code. However, we observe that the overhead of calling numba compiled functions is heavy, and the numba's `object mode` is slow.

object mode is 1.6x slower than CPython:
```python
def add1(x):
   return x + 1
```

object mode is 70x slower when `x` is a 1000-length sequence:
```python
def pysum(x):
   s = 0
   for i in x:
      s += i
   return s
```

So use of Numba is really restricted.

## How to develop

### A Gradually Growing Python JIT

Many guys working on Python JIT tries to make a big progress in the very beginning, however this is not realistic according to the history.

...

### Runtime CodeGen and Specializations

This great idea makes a lot of sense, and is quite easy to implement.

`psyco` seems to be a early promoter for this method, and did achieve some progress.

Runtime specializations uses the runtime types to dispatch.

See, for this generic code

```python
def flat_rep(x, n):
   res = x
   while n:
      res += some_function(x)
      n -= 1
   return x
```

When calling `flat_rep` during runtime, it's okay for us to use the types of arguments to select an optimized method.

An optimized method for specific types is also generated in runtime, like `flat_rep(str, int) -> str` and `flat_rep(int, int) -> int`.


Why we will benefit from runtime specializations and runtime codegen?

See `flat_rep`, when calling it, 
- we **must be able to** specialize `flat_rep`, and 
- we might be able to specialize `some_function` which is invoked inside `flat_map`, and
- we might also be able to specialize inner function calls happened inside `some_function`,
- ...
We got it!

Notice that, the possibility of specializing inner function calls, is just depending on how we infer the types, so we need runtime type inference! Julia programming language just shows this is practical.


However, the difference between Python and Julia is significant even in terms of the potentials of specializations:

Python does not have a "canonical" type system. Honestly, Python, as a classic dynamic language, does not have a type system.

The Python type of a datum, does not describe the specification of all stuffs kept in the datum.

See, for this generic code
```python
def sum_it(seq, zero):
   res = zero
   for each in seq:
      res += each
   return res
```

`sum_it([1, 2, 3], 0)` produces `sum_it(list, int) -> int`.

`sum_float([1.0, 2.0, 3.0], 0.0)` produces `sum_it(list, float) -> float`.

By using Python types, we can successfully dispatch by using the type of the second parameter.

However!
However!

We CANNOT code for the first parameter, because it can be always a list.

So to make JIT more impressive, we need design a better runtime representations.


(TODO)