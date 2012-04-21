module Beagle.Domain
    ( Gene(..)
    , genemap
    ) where

import Data.Dynamic
import qualified Data.Map as Map

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
          | Multiple
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
    (Add,       toDyn add),
    (Subtract,  toDyn subtract),
    (Multiply,  toDyn multiply),
    (Divide,    toDyn divide),
    (Stop,      toDyn stop),
    ]
 
digit :: Int -> Maybe Int -> Maybe Int
digit x Nothing = Just x
digit x (Just y) = Just (read (show x ++ show y))

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

-- | Maybe Int -> Maybe Int -> Maybe Int
add      (Just x) (Just y) = Just (x + y)
subtract (Just x) (Just y) = Just (x - y)
multiply (Just x) (Just y) = Just (x * y)
divide   (Just x) (Just y) = Just (x / y)

stop = Nothing :: Maybe Int

