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