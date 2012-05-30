module Beagle.Domain
    ( Gene(..)
    , genotypeLength
    , populationSize
    , randomSeed
    , score
    ) where

import Beagle.Eval
import qualified Data.Map as Map

-- | Data types and constants --------------------------------------------------

genotypeLength =        50
populationSize =        100     :: Int
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
    (Decr,          decr'),
    (Empty,         empty)
    ]

-- | Math genes ----------------------------------------------------------------

add', subtract', multiply', divide', incr', decr', empty :: 
    [Gene] -> Int -> State -> ([Gene], Int, State)

add' gs gi (a, b)       = (gs, gi+1, (a+b, 0))
subtract' gs gi (a, b)  = (gs, gi+1, (max 0 (a-b), 0))
multiply' gs gi (a, b)  = (gs, gi+1, (a*b, 0))
divide' gs gi (a, 0)    = (gs, gi+1, (0, 0))
divide' gs gi (a, b)    = (gs, gi+1, (quot a b, 0))
incr' gs gi (a, b)      = (gs, gi+1, (a, b+1))
decr' gs gi (a, b)      = (gs, gi+1, (a, max 0 (b-1)))
empty gs gi s           = (gs, gi+1, s)


-- | Control flow genes --------------------------------------------------------

{-
jumpUnlessFlagAnd, jumpUnlessFlagOr, jumpUnlessFlagTrue, jumpUnlessFlagFalse, 
flagSwitch, jumpHere, jump
    :: [Gene] -> Int -> State -> ([Gene], Int, State)

jumpHere gs gi s = (gs, gi+1, s)
flagSwitch gs gi (fa, fb, fs, ba, bb, grid) = (gs, gi+1, (fa, fb, not fs, ba, bb, grid)) 

jumpUnlessFlagAnd gs gi (True, True, fs, ba, bb, grid) = (gs, gi+1, (fa, fb, fs, ba, bb, grid))
jumpUnlessFlagAnd gs gi s = jump 1 ga (gi+1) s 

jumpUnlessFlagOr gs gi (True, fb, fs, ba, bb, grid) = (gs, gi+1, (fa, fb, fs, ba, bb, grid))
jumpUnlessFlagOr gs gi (fa, True, fs, ba, bb, grid) = (gs, gi+1, (fa, fb, fs, ba, bb, grid))
jumpUnlessFlagOr gs gi s = jump 1 ga (gi+1) s 

jumpUnlessFlagTrue gs gi (True, fb, fs, ba, bb, grid) = (gs, gi+1, (fa, fb, fs, ba, bb, grid))
jumpUnlessFlagTrue gs gi s = jump 1 ga (gi+1) s 

jumpUnlessFlagFalse gs gi (False, fb, fs, ba, bb, grid) = (gs, gi+1, (fa, fb, fs, ba, bb, grid))
jumpUnlessFlagFalse gs gi s = jump 1 ga (gi+1) s 

jump count gs gi s
    | count == 0 || gi == length gs -1 = gs gi s
    | otherwise = case gs !! gi of
                    JumpUnlessFlagAnd ->    jump (count+1) (gi+1) s
                    JumpUnlessFlagOr ->     jump (count+1) (gi+1) s
                    JumpUnlessFlagTrue ->   jump (count+1) (gi+1) s
                    JumpUnlessFlagFalse ->  jump (count+1) (gi+1) s
                    JumpHere ->             jump (count-1) (gi+1) s
                    _ -> jump count gs (gi+1) s
-}

-- | Scoring function ----------------------------------------------------------

score :: [Gene] -> IO Float
score gs = do
        finalState <- eval gs gmap 0 initState
        return (score' finalState)
    where score' = norm . fromIntegral . abs . (123-) . fst
          initState = (0, 0)
          norm x = 1 - (1/(x+1)) -- not linear!

