module Beagle.Domain
    ( Gene(..)
    , gmap
    , genotypeLength
    , mutationsPerGenotype
    , populationSize
    , randomSeed
    , score
    , initState
    , State
    ) where

import qualified Data.Map as Map
import Debug.Trace
import System.IO.Unsafe
import System.Random

genotypeLength =        15      :: Int
mutationsPerGenotype =  1       :: Int
populationSize =        30      :: Int
randomSeed =            6       :: Int

data Gene = RotateR | RotateL | SwapR | SwapL | MoveR | MoveL | Empty
    deriving (Ord, Eq, Show, Enum)

gmap :: Map.Map Gene ([Gene] -> Int -> State -> ([Gene], Int, State))
gmap = Map.fromList [
    (RotateR,       rotateR),
    (RotateL,       rotateL),
    (SwapR,         swapR),
    (SwapL,         swapL),
    (MoveR,         moveR),
    (MoveL,         moveL),
    (Empty,         empty)
    ]

type State = (Int, [Int])

rotateR, rotateL, swapR, swapL, moveR, moveL, empty :: 
    [Gene] -> Int -> State -> ([Gene], Int, State)

rotateR gs gi (i, xs) = (gs, gi+1, (i, last xs: init xs))
rotateL gs gi (i, xs) = (gs, gi+1, (i, tail xs ++ [head xs]))
swapR gs gi (i, xs) 
    | i == length xs -1 = (gs, gi+1, (i, xs))
    | otherwise = let (bxs, x:x':axs) = splitAt i xs
                  in (gs, gi+1, (i, bxs ++ (x':x:axs)))
swapL gs gi (i, xs) 
    | i == 0 = (gs, gi+1, (i, xs))
    | otherwise = let (bxs, x:axs) = splitAt i xs
                      x' = last bxs
                      bxs' = init bxs
                  in (gs, gi+1, (i, bxs' ++ (x:x':axs)))
moveR gs gi (i, xs)
    | i == length xs -1 = (gs, gi+1, (0, xs))
    | otherwise = (gs, gi+1, (i+1, xs))
moveL gs gi (i, xs)
    | i == 0 = (gs, gi+1, (length xs -1, xs))
    | otherwise = (gs, gi+1, (i-1, xs))
empty gs gi s = (gs, gi+1, s)

score :: State -> Float
score (_, xs) = score' $ traceShow xs xs
    where score' = (/10) . fromIntegral . length . filter (\(a, b) -> a /= b) . 
            zip [1..]

initState :: (Int, [Int])
initState = (0, [1, 3, 5, 2, 6, 4])

