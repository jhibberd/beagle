module Beagle.Domain
    ( Gene(..)
    , genemap
    , stop
    , genotypeLength
    , targetPhenotype
    , mutationsPerGenotype
    , populationSize
    , randomSeed
    , numSolutions
    ) where

import Data.Dynamic
import qualified Data.Map as Map

genotypeLength =        6   :: Int
targetPhenotype =       55  :: Float
mutationsPerGenotype =  2   :: Int
populationSize =        10  :: Int
randomSeed =            6   :: Int
numSolutions =          1   :: Int

{- Board positions are labelled as follows:
   
    A | B | C
   -----------
    D | E | F
   -----------
    G | H | I

-}

data Gene = Play
          | If
          | Not
          | TakenByMe
          | TakenByThem
          | A
          | B
          | C
          | D
          | E
          | F
          | G
          | H
          | I
          | Empty
          deriving (Ord, Eq, Show, Enum)

genemap :: Map.Map Gene Dynamic
genemap = Map.fromList [
    (Play,          toDyn play),
    (IfCondition,   toDyn ifCondition),
    (Not,           toDyn not),
    (TakenByMe,     toDyn takenByMe),
    (TakenByThem,   toDyn takenByThem),
    (A,             toDyn A),
    (B,             toDyn B),
    (C,             toDyn C),
    (D,             toDyn D),
    (E,             toDyn E),
    (F,             toDyn F),
    (G,             toDyn G),
    (H,             toDyn H),
    (I,             toDyn I),
    ]

data Position = A | B | C | D | E | F | G | H | I deriving (Enum)
data State = Free | Them | Me
type Board = [State]

play :: Board -> Position -> Board
play b p = map (\x i -> if i == p then Me else x) . zip b [A..I] 

ifCondition :: Bool -> a -> b -> c
ifCondition True a _ = $ a
ifCondition False _ b = $ b

takenByMe :: Board -> Position -> Bool
takenByMe b p = (b !! (formEnum p)) == Me

takenByThem :: Board -> Position -> Bool
takenByThem b p = (b !! (formEnum p)) == Them

