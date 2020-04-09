First-class Pattern Matching in the Final Approach
==========================================================


Core Ideas
------------------

In today's FP seminar, I refined the idea in my mind, which is directly responsible for

- another **Pattern-matching in the final approach** not noted at [http://okmij.org/ftp/tagless-final/#course-oxford](http://okmij.org/ftp/tagless-final/#course-oxford)
- first-class pattern matching(for lazy programming language only..)

The core idea is,

1. avoid using type classes to express tagless final, instead, follow OCaml's implementations(`SYM`, `SYMShow`, `sym`(by higher rank types), `SYMSelf` mentioned in [http://okmij.org/ftp/tagless-final/](http://okmij.org/ftp/tagless-final/))

2. code shall look this way:
   ```haskell
   match (succ zero) $ defaultMatcher
    { zero_m = error "non-exhausted"
    , succ_m = \_ -> "it's succ case!"
    }
   -- => "it's succ case!"
   ```

Full implementation in Haskell
---------------------------------------

(p.s: initially I use PureScript, Row types are nice)


```haskell
{-# LANGUAGE RankNTypes #-}
module Main where

-- Signature
data SYM repr
    = SYM
    { add_m :: repr -> repr -> repr
    , neg_m :: repr -> repr
    , lit_m :: Bool -> repr
    }

-- above definition is similar to the following one
-- class SYM repr where
--     add :: repr -> repr -> repr
--     neg :: repr -> repr
--     lit :: Bool -> repr
-- 
-- however, encoding signatures with data types, makes it possible for us
-- to achieve more first-class things

type Self = forall repr. SYM repr -> repr

lit :: Bool -> Self
lit bool mod = lit_m mod bool

neg :: Self -> Self
neg term mod = neg_m mod (term mod)

add :: Self -> Self -> Self
add t1 t2 mod = add_m mod (t1 mod) (t2 mod)

-- module 1: self algebra
-- however, not compiled...
-- self :: SYM Self
-- self = SYM 
--     { lit_m = lit
--     , neg_m = neg
--     , add_m = add
--     }

-- module 2: string algebra
symShow :: SYM String
symShow = SYM 
    { add_m = \l r -> "(" ++ l ++ " + " ++ r ++ ")"
    , neg_m = ("-" ++)
    , lit_m = show
    }

-- module 3: evaluator
symEval :: SYM Bool
symEval = SYM 
    { add_m = (&&)
    , neg_m = not
    , lit_m = id
    }

data Init = Add Init Init | Lit Bool | Neg Init
    deriving (Show)

-- module 4: to ADT
symInit :: SYM Init
symInit = SYM
    { add_m = Add
    , neg_m = Neg
    , lit_m = Lit
    }


-- pattern matching operation for elements in self algebras
matchTerm :: Self -> Self
matchTerm term matcher = term matcher


nonExhausted = error
-- a default pattern matching cases
-- look, this is first class pattern matching!
defaultMatcher :: forall repr. SYM repr
defaultMatcher = SYM 
    { lit_m = nonExhausted "literal"
    , add_m = nonExhausted "addition"
    , neg_m = nonExhausted "negation"
    }

main :: IO ()
main = do
  
  putStrLn $ term1 symShow  -- => True
  print $ term2 symEval     -- => False
  print $ term2 symInit     -- => Add (Lit False) (Lit True)

  putStrLn $ matchTerm term1 $ defaultMatcher
    { lit_m = const "it's a literal!"}
  -- => it's a literal!

  putStrLn $ matchTerm term2 $ defaultMatcher
    { add_m = \_ _ -> "it's an addition!"}
  -- => it's an addition!
  where
      term1 = lit True
      term2 = lit False `add` lit True
```


Q: Think, why this works for LAZY language only?

A: Because initial algebra of tagless final implies pre-order visit.

Hence, currently, it works only for Haskell, but not for PureScript, even type checked for the latter.
