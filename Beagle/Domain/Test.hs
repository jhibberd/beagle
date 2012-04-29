module Beagle.Domain
    ( Gene(..)
    , genotypeLength
    , mutationsPerGenotype
    , populationSize
    , randomSeed
    , score
    ) where

import Beagle.Eval2
import qualified Data.Map as Map

-- | Data types and constants --------------------------------------------------

genotypeLength =        15      :: Int
mutationsPerGenotype =  1       :: Int
populationSize =        30      :: Int
randomSeed =            6       :: Int

data Gene = Add | Subtract | Multiply | Divide | Incr | Decr | Empty
    deriving (Ord, Eq, Show, Enum)
type State = (Int, Int)

gmap :: Map.Map Gene ([Gene] -> Int -> State -> ([Gene], Int, State))
gmap = Map.fromList [
    (Add,           add'),
    (Subtract,      subtract'),
    (Multiply,      multiply'),
    (Divide,        divide'),
    (Incr,          incr'),
    (Decr,          decr')
    ]

-- | Gene implementations ------------------------------------------------------

add', subtract', multiply', divide', incr', decr' :: 
    [Gene] -> Int -> State -> ([Gene], Int, State)

add' gs gi (a, b) = (gs, gi+1, (a+b, 0))
subtract' gs gi (a, b) = (gs, gi+1, (a-b, 0))
multiply' gs gi (a, b) = (gs, gi+1, (a*b, 0))
divide' gs gi (a, 0) = (gs, gi+1, (0, 0))
divide' gs gi (a, b) = (gs, gi+1, (quot a b, 0))
incr' gs gi (a, b) = (gs, gi+1, (a, b+1))
decr' gs gi (a, b) = (gs, gi+1, (a, min 0 (b-1)))

-- | Scoring function ----------------------------------------------------------

score :: [Gene] -> Float
score gs = score' $ eval gs gmap 0 initState
    where score' = fromIntegral . fst
          initState = (0, 0)

