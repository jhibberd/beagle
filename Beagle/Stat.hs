-- | Maintain counters of events during application of the genetic algorithm
-- in order to produce statistics on its performance.

module Beagle.Stat 
    ( Counters
    , newCounters
    , incrGenotypes
    , incrSolutions
    , stats
    ) where

import qualified Data.Map as Map

{- Counters:
   0 - Number of generated genotypes
   1 - Number of solution genotypes found
-}
type Counters = [Int]

newCounters :: [Int]
newCounters = replicate 2 0

-- | Calculate stats given a list of counters.
stats :: Counters -> Map.Map String Int
stats cs = Map.fromList [
    ("average_num_genotypes_per_solution", quot (cs !! 0) (cs !! 1)),
    ("solutions", cs !! 1)
    ]

modifyCounter :: Int -> (Int -> Int) -> Counters -> Counters
modifyCounter i f s = let (before, after) = splitAt i s
                      in before ++ (f (head after) : tail after)

incrGenotypes :: Int -> Counters -> Counters
incrGenotypes n = modifyCounter 0 (+n) 

incrSolutions :: Int -> Counters -> Counters
incrSolutions n = modifyCounter 1 (+n)

