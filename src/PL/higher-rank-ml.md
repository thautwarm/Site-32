# SMLFS: ML with

- **Higher rank type**!!
- Type classes!!
- Scoped type variables
- Wildcard types(Type holes)

## A simple type system extended from HM to support HRTs

The idea is simple, when we analyze ASTs, we firstly perform the analysis for names, etc, but the type 
inference is delayed in some degree:

```ocaml
val infer_term : term -> type_check_ctx -> (topdown_type_info -> typed_term)
```

The above signature says that, after you performed `infer_term`, there is another chance for you
to propagate type information bidirectionally.

**bidirection** here:

1. in the first iteration of type analysis(calling `infer_term`), annotations of names can
be assigned to terms bottom-up.

2. in the second iteration(how you manage to call `topdown_type_info -> typed_term`), you propagate
type information a in top-down way.


The idea is this simple...

```ocaml
type topdown_type_info =
| InstantiateTo of hindley_milner_type
| NoInfo
```

In some cases, there can be no type annotations from the *topper* level, in this case,
the algorithm is nothing different from unification and infer rules of vanilla HM type system.

However, leveraging `topdown_type_info` requires a different unification rule,
we'd call it `unifyINST`.

`unifyINST` means, the LHS can be instantiated from the RHS, e.g.,

```
'a : monotype
unifyINST ('a -> 'a)  (forall a. a -> a) == true
unifyINST int  (forall a. a) == true
```


##
