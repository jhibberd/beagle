module Beagle.Domain
    ( Gene(..)
    , genemap
    , stop
    , genotypeLength
    , targetPhenotype
    , mutationsPerGenotype
    , populationSize
    , randomSeed
    ) where

import Data.Dynamic
import qualified Data.Map as Map

genotypeLength =        7   :: Int
targetPhenotype =       55  :: Float
mutationsPerGenotype =  2   :: Int
populationSize =        10  :: Int
randomSeed =            5   :: Int

data Gene = Digit0
          | Digit1
          | Digit2
          | Digit3
          | Digit4
          | Digit5
          | Digit6
          | Digit7
          | Digit8
          | Digit9
          | Add
          | Subtract
          | Multiply
          | Divide
          | Stop
          | Empty
          deriving (Ord, Eq, Show, Enum)

genemap :: Map.Map Gene Dynamic
genemap = Map.fromList [
    (Digit0,    toDyn digit0),
    (Digit1,    toDyn digit1),
    (Digit2,    toDyn digit2),
    (Digit3,    toDyn digit3),
    (Digit4,    toDyn digit4),
    (Digit5,    toDyn digit5),
    (Digit6,    toDyn digit6),
    (Digit7,    toDyn digit7),
    (Digit8,    toDyn digit8),
    (Digit9,    toDyn digit9),
    (Add,       toDyn add'),
    (Subtract,  toDyn subtract'),
    (Multiply,  toDyn multiply'),
    (Divide,    toDyn divide'),
    (Stop,      toDyn stop)
    ]
 
digit :: Int -> Maybe Int -> Maybe Int
digit x Nothing = Just x
digit x (Just y) = Just (read v)
    where v = show (max x 0) ++ show (max y 0)

digit0 = digit 0
digit1 = digit 1
digit2 = digit 2
digit3 = digit 3
digit4 = digit 4
digit5 = digit 5
digit6 = digit 6
digit7 = digit 7
digit8 = digit 8
digit9 = digit 9

add' :: Maybe Int -> Maybe Int -> Maybe Int
add' (Just x) (Just y) = Just (x + y)
add' _ _ = Nothing

subtract' :: Maybe Int -> Maybe Int -> Maybe Int
subtract' (Just x) (Just y) = Just (x - y)
subtract' _ _ = Nothing

multiply' :: Maybe Int -> Maybe Int -> Maybe Int
multiply' (Just x) (Just y) = Just (x * y)
multiply' _ _ = Nothing

divide' :: Maybe Int -> Maybe Int -> Maybe Int
divide' (Just _) (Just 0) = Just 0
divide' (Just x) (Just y) = Just (quot x y)
divide' _ _ = Nothing

stop = Nothing :: Maybe Int

