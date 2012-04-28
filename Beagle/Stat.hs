-- | Maintain counters of events during application of the genetic algorithm
-- in order to produce statistics on its performance.

module Beagle.Stat 
    ( Counters
    , newCounters
    , incrGenotypes
    , incrSolutions
    ) where

{- Counters:
   0 - Number of generated genotypes
   1 - Number of genotypes that couldn't be evaluated
-}
type Counters = [Int]

newCounters :: [Int]
newCounters = replicate 2 0

incrGenotypes :: Int -> Counters -> Counters
incrGenotypes n = modifyCounter 0 (+n) 

incrSolutions :: Int -> Counters -> Counters
incrSolutions n = modifyCounter 1 (+n)

modifyCounter :: Int -> (Int -> Int) -> Counters -> Counters
modifyCounter i f s = let (before, after) = splitAt i s
                      in before ++ (f (head after) : tail after)
