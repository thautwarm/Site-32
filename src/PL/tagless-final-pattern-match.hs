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

newtype Self = Self (forall repr. SYM repr -> repr)

lit :: Bool -> Self
lit bool = Self $ \mod -> lit_m mod bool

neg :: Self -> Self
neg (Self term) = Self $ \mod -> neg_m mod (term mod)

add :: Self -> Self -> Self
add (Self t1) (Self t2) = Self $ \mod -> add_m mod (t1 mod) (t2 mod)

-- module 1: self algebra
self :: SYM Self
self = SYM
    { lit_m = lit
    , neg_m = neg
    , add_m = add
    }

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
runTerm :: Self -> forall repr. SYM repr -> repr
runTerm (Self term) matcher = term matcher

matchTerm = runTerm

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
  
  putStrLn $ runTerm term1 symShow  -- => True
  print $ runTerm term2 symEval     -- => False
  print $ runTerm term2 symInit     -- => Add (Lit False) (Lit True)

  putStrLn $ matchTerm term1 $ defaultMatcher
    { lit_m = const "it's a literal!"}
  -- => it's a literal!

  putStrLn $ matchTerm term2 $ defaultMatcher
    { add_m = \_ _ -> "it's an addition!"}
  -- => it's an addition!
  where
      term1 = lit True
      term2 = lit False `add` lit True
