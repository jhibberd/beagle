-- | Simple math-based domain to use in unit-tests.

module Beagle.Domain
    ( Gene(..)
    , genemap
    , genotypeLength
    , targetPhenotype -- TODO(jhibberd) Do we need this?
    , mutationsPerGenotype
    , populationSize
    , randomSeed
    , runner
    ) where

import Data.Dynamic
import qualified Data.Map as Map

genotypeLength =        8   :: Int
targetPhenotype =       10   :: Int
mutationsPerGenotype =  1   :: Int
populationSize =        50  :: Int
randomSeed =            6   :: Int

data Gene = Digit1   -- 1
          | Digit2   -- 2
          | Digit3   -- 3
          | Plus     -- Add two digits together
          | Minus    -- Subtract one digit from another
          -- | Stick    -- Concat two digits, eg. f 1 3 = 13
          | Switch   -- Takes three ints; returns one of two ints, depending on
                     -- the value of a third.
          -- | Runner   -- Takes a function and applies it to an arbitrary int.
                     -- Used for an evaluation function applied to the end of a
                     -- genotype.
          -- | Runner'  -- Takes an int and returns the difference between it and a 
                     -- target int.
          -- | Runner'' -- Convert final int value to float; useful for testing.
          | Empty    -- Gene to be ignored; allows for genotypes of 'variable'
                     -- length.
          deriving (Ord, Eq, Show, Enum)

genemap :: Map.Map Gene (Dynamic, Int)
genemap = Map.fromList [
    (Digit1,        (toDyn digit1,      0)),
    (Digit2,        (toDyn digit2,      0)),
    (Digit3,        (toDyn digit3,      0)),
    (Plus,          (toDyn plus,        2)),
    (Minus,         (toDyn minus,       2)),
    --(Stick,         (toDyn stick,       2)),
    (Switch,        (toDyn switch,      3))
    --(Runner,        (toDyn runner,      1)),
    --(Runner',       (toDyn runner',     1)),
    --(Runner'',      (toDyn runner'',    1))
    ]

runner =            (toDyn runner',     1 :: Int)

digit1, digit2, digit3 :: Int
digit1 = 1
digit2 = 2
digit3 = 3

plus, minus, stick :: Int -> Int -> Int
plus x y = x + y
minus x y = x - y
stick x y = read (show x ++ show y) 

switch :: Int -> Int -> Int -> Int
switch a b x 
    | x > 0 = a
    | otherwise = b

{-
runner :: (Int -> Int) -> Float
runner f = fromIntegral (f 100)
-}

runner' :: Int -> Float
runner' x = fromIntegral $ abs (x - targetPhenotype) 

runner'' :: Int -> Float
runner'' x = fromIntegral x

