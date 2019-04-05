
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

import Control.Arrow
import Data.Ratio
import Data.Kind

newtype MkTest (c :: * -> Constraint) a = MkTest {runTest :: a}

class TestCase (c :: * -> Constraint) a where
    samples      :: c a => MkTest c [a]
    testWith     :: (c a => (a -> Bool)) -> (c a => MkTest c [(a, Bool)])
    testWith logic =
        MkTest $ map (id &&& logic) seq
        where
            seq :: [a]
            seq = runTest (samples :: MkTest c [a])

type TestOn c a = c a => (a -> Bool) -> MkTest c [(a, Bool)]
type Test c a = c a => MkTest c [(a, Bool)]

instance TestCase Enum a where
    samples = MkTest . enumFrom . toEnum $ 0


instance TestCase Bounded a where
    samples = MkTest [maxBound, minBound]


onEnumerable :: TestOn Enum a
onEnumerable = testWith

data MyData = A | B | C deriving (Eq, Enum, Show)

main :: IO ()
main = do
    putStrLn . show . take 10 . runTest $ testWith logicForInt
    putStrLn . show . runTest $ testWith logicForMyD
    putStrLn . show . runTest $ testOnBoundInt
    return ()
    where
        testOnBoundInt :: Test Bounded Int
        testOnBoundInt = testWith logicForInt
        logicForInt x = (x - 2)^2 ==  (x^2 - 4*x + 4)
        logicForMyD A = A == toEnum 0
        logicForMyD B = B == toEnum 1
        logicForMyD C = C == toEnum 2
