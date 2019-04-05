import Data.Ratio

class Countable a where
    counts :: [a]

instance Integral a => Countable (Ratio a) where
    counts =
        plane
        where
        ints = [1..]
        pointGen tail (x, y) = (x % y) : (-x % y) : tail
        lineGen tail n =
                 let points = [(i, j) | i <- [1 .. n], j <- [1 .. n - i]]
                 in foldl pointGen tail points
        plane = 0 : recurse 1
                where recurse n = lineGen (recurse $ n + 1) n

z :: [Ratio Integer]
z = counts

main :: IO ()
main = do
    putStrLn . show $ take 10 z
    return ()
