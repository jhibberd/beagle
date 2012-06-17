-- | Attempt to generate all 827 base cases for the game of TicTacToe.

import Data.HashTable hiding (toList)
import Data.List
import Prelude hiding (flip)

type Grid = (Int, Int, Int, Int, Int, Int, Int, Int, Int)

-- | Given any grid scenario return all 8 symmetrical variations.
variants :: Grid -> [Grid]
variants g = rotateAll g ++ rotateAll (flip g)
    where rotateAll g = let g1 = rotate g
                            g2 = rotate g1
                            g3 = rotate g2
                        in [g, g1, g2, g3]

-- | Rotate a grid 90 degrees clockwise.
-- 
-- a b c      g d a
-- d e f  ->  h e b
-- g h i      i f c
rotate :: Grid -> Grid
rotate (a, b, c, d, e, f, g, h, i) = (g, d, a, h, e, b, i, f, c)

-- | Flip a grid over its horizontal axis.
--
-- a b c      g h i
-- d e f  ->  d e f
-- g h i      a b c
flip :: Grid -> Grid
flip (a, b, c, d, e, f, g, h, i) = (g, h, i, d, e, f, a, b, c)

-- | Pretty-print a grid.
pprint :: Grid -> IO ()
pprint (a, b, c, d, e, f, g, h, i) = do
        printLine a b c
        printLine d e f
        printLine g h i
        putStrLn ""
    where printLine x y z = putStrLn $ mark x ++ " " ++ mark y ++ " " ++ mark z
          mark (-1) = "O"
          mark 1 =  "X"
          mark 0 =  "."

-- | Express a grid as a hashable int.
toInt :: Grid -> Int
toInt g = foldr f 0 (zip [0..] (allPositive g))
    where f (i, x) b = b + (x * (10 ^ i))
          allPositive = map (\x -> if x == (-1) then 2 else x) . toList

-- | Convert a grid (tuple) to a list.
toList :: Grid -> [Int]
toList (a, b, c, d, e, f, g, h, i) = [a, b, c, d, e, f, g, h, i]

-- | Given a list of 8 grid variations pick the base case (the one with the
-- lowest hash value.
base :: [Grid] -> Grid
base = snd . head . sort . map f 
    where f x = (hashInt $ toInt x, x)

main = do
    --mapM_ pprint $ variants (0, 0, 1, (-1), (-1), 0, 1, 0, 0)
    print . base $ variants (0, 0, 1, (-1), (-1), 0, 1, 0, 0)

