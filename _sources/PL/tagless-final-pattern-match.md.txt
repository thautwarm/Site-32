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


Enhancement 1
---------------------------------

To avoid **impredicative polymorphisms** and support **GADTs**:

```haskell
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

data Exp a where
    IfExp :: Exp Bool -> Exp a -> Exp a -> Exp a
    Lam :: (a -> b) -> Exp (a -> b)
    App :: Exp (a -> b) -> Exp a -> Exp b
    Val :: a -> Exp a


-- Signature
data SYM repr =
    SYM
    { ifExp_m :: repr Bool -> forall a. repr a -> repr a -> repr a
    , lam_m :: forall a b. (a -> b) -> repr (a -> b)
    , app_m :: forall a b. repr (a -> b) -> repr a -> repr b
    , val_m :: forall a. a -> repr a
    }

newtype Self a = Self (forall repr. SYM repr -> repr a)

ifExp :: Self Bool -> forall a. Self a -> Self a -> Self a
ifExp (Self bool) (Self t) (Self f) = Self $ \mod -> ifExp_m mod (bool mod) (t mod) (f mod)

lam :: (a -> b) -> Self (a -> b)
lam f = Self $ \mod -> lam_m mod f

app :: Self (a -> b) -> Self a -> Self b
app (Self f) (Self arg) = Self $ \mod -> app_m mod (f mod) (arg mod)

val :: a -> Self a
val v = Self $ \mod -> val_m mod v

-- module 1: self algebra
self :: SYM Self
self = SYM
    { ifExp_m = ifExp
    , lam_m = lam
    , app_m = app
    , val_m = val
    }

-- module 4: to ADT
symInit :: SYM Exp
symInit = SYM
    { ifExp_m = IfExp
    , lam_m = Lam
    , val_m = Val
    , app_m = App
    }

-- pattern matching operation for elements in self algebras
runTerm :: Self a -> forall repr. SYM repr -> repr a
runTerm (Self term) matcher = term matcher

matchTerm = runTerm

nonExhausted = error
-- a default pattern matching cases
-- look, this is first class pattern matching!
defaultMatcher :: forall repr. SYM repr
defaultMatcher = SYM 
    { lam_m = nonExhausted "lambda"
    , app_m = nonExhausted "application"
    , val_m = nonExhausted "value"
    , ifExp_m = nonExhausted "if expression"
    }

evalInit :: Exp a -> a
evalInit = \case
    Lam f -> f
    App f a -> evalInit f (evalInit a)
    IfExp cond t f -> evalInit (if evalInit cond then t else f)
    Val v -> v

newtype C a = C a deriving (Show)
evalFinal :: SYM C
evalFinal = SYM
   { ifExp_m = \(C cond) e f -> if cond then e else f
   , app_m = \(C f) (C arg) -> C (f arg)
   , val_m = C
   , lam_m = C
   }
main :: IO ()
main = do
  let result :: forall repr. SYM repr -> repr _
      result = runTerm term1
  print $ evalInit (result symInit)
  print $ result evalFinal
  where
      term1 = app (lam $ \x -> x + 1) (val 1)
-- =>
-- 2
-- C 2
```