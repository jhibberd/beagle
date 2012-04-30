-- | Nondeterministic functions that return values with an element of 
-- randomness.

module Beagle.Random
    ( gene
    , map
    , pick
    , g
    ) where

import qualified Beagle.Domain as D
import Prelude hiding (map)
import System.Random

-- | Class implementations to simply the production of random genes.

instance Bounded D.Gene where
    minBound = toEnum 0
    maxBound = last $ [minBound :: D.Gene ..]

instance Random D.Gene where
    randomR (a,b) g = 
        case (randomR (fromEnum a, fromEnum b) g) of
            (x, g') -> (toEnum x, g')
    random g = randomR (minBound, maxBound) g

-- | Return random generator.
g :: StdGen
g = mkStdGen D.randomSeed

-- | Return a random gene.
gene :: RandomGen g => g -> (D.Gene, g)
gene g = randomR (minBound, maxBound) g

-- | Return n random elements from a list.
pick :: RandomGen g => [a] -> Int -> g -> ([a], g)
pick _ 0 g = ([], g)
pick xs n g = let (ri, g') =   randomR range g
                  (xs', g'') = pick (lsdrop xs ri) (n-1) g'
              in ((xs !! ri):xs', g'')
    where range = (0, (length xs)-1)

-- | Apply a function (potentially containing "randomness") to n random elements
-- of a list.
--
-- The function first picks n random index positions within the list (hotxs). 
-- Then zips each element value with its index position and passes the composite
-- list to an inner function (m). The inner function iterates over the composite
-- list and applies the function (f) to all elements (x) whose index (i) appers
-- in the random index list (now, hxs).
map :: RandomGen g 
    => (a -> g -> (a, g))
    -> [a]
    -> Int
    -> g
    -> ([a], g)
map _ [] _ g = ([], g)
map f xs n g = let (hotxs, g') = pick [0..(length xs)-1] n g 
               in m f (zip xs [0..]) hotxs g'
    where m :: RandomGen g
               => (a -> g -> (a, g)) 
               -> [(a, Int)] 
               -> [Int]
               -> g 
               -> ([a], g)
          m _ [] _ g = ([], g)
          m f ((x, i):xs) hxs g
              | elem i hxs    = let (x', g') =    f x g
                                    (xs', g'') =  m f xs hxs g'
                                in  (x':xs', g'')
              | otherwise     = let (xs', g') =   m f xs hxs g
                                in  (x:xs', g')

-- | Remove element at a specific index position from a list.
-- NOTE: Not really a random function but doesn't belong anywhere else and 
-- currently only used internally within this module.
lsdrop :: [a] -> Int -> [a]
lsdrop [] _ = []
lsdrop xs i = let (a, b) = splitAt i xs in a ++ (drop 1 b)

