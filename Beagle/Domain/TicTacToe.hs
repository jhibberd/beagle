{-
module Beagle.Domain
    ( Gene(..)
    , genemap
    , stop
    , genotypeLength
    , mutationsPerGenotype
    , populationSize
    , randomSeed
    , runner
    ) where
-}

import Data.Dynamic
import qualified Data.Map as Map
import System.Random
import Debug.Trace
import System.IO.Unsafe

genotypeLength =        6   :: Int
mutationsPerGenotype =  2   :: Int
populationSize =        10  :: Int
randomSeed =            6   :: Int

{- Board positions are labelled as follows:
   
    A | B | C
   -----------
    D | E | F
   -----------
    G | H | I

-}

{-
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
-}
{-
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
    (I,             toDyn I)
    ]
-}

data Position = A | B | C | D | E | F | G | H | I deriving (Enum)
data State = Free | X | O deriving (Eq, Show)
type Board = [State]
data GameState = Draw | XWins | OWins | Open deriving (Show)
data Turn = XTurn | OTurn

{-
play :: Board -> Position -> Board
play b p = map (\x i -> if i == p then Me else x) . zip b [A..I] 


takenByMe :: Board -> Position -> Bool
takenByMe b p = (b !! (formEnum p)) == Me

takenByThem :: Board -> Position -> Bool
takenByThem b p = (b !! (formEnum p)) == Them
-}

-- TODO(jhibberd) Throw in lots of functions that could be useful and let the
-- natural selection process pick the ones it finds most useful.

playA :: Board -> Board
playA b = play O b 0

isAX :: Board -> Bool
isAO :: Board -> Bool

branch :: Bool -> Board -> Board -> Board

-- | Fitness function mechanics ------------------------------------------------

play :: State -> Board -> Int -> Board
play t b p 
    | b !! p /= Free = error "Position not free."
    | otherwise = let (x,_:xs) = splitAt p b
                  in (x ++ (t:xs))

getState :: Board -> GameState
getState b
    | wins b = XWins
    | wins (reverseb b) = OWins
    | filter (== Free) b == [] = Draw
    | otherwise = Open
    
reverseb :: Board -> Board
reverseb = map (\x -> case x of
                    X -> O
                    O -> X
                    x -> x)

wins :: Board -> Bool
wins [X, X, X, _, _, _, _, _, _] = True
wins [_, _, _, X, X, X, _, _, _] = True
wins [_, _, _, _, _, _, X, X, X] = True
wins [X, _, _, X, _, _, X, _, _] = True
wins [_, X, _, _, X, _, _, X, _] = True
wins [_, _, X, _, _, X, _, _, X] = True
wins [X, _, _, _, X, _, _, _, X] = True
wins [_, _, X, _, X, _, X, _, _] = True
wins _ = False

hostPlay :: (RandomGen g) => Board -> g -> (Board, g)
hostPlay b g = let (i, g') = randomR (0, (length freeonly)-1) g
                   i'' = freeonly !! i
               in (play X b i'', g')
    where zipped = zip b [0..]
          freeonly = map snd $ filter ((==Free) . fst) zipped

-- | TODO(jhibberd) This function will be substituted for the genotype.
clientPlay :: Board -> Board
clientPlay b =
    let i = read .unsafePerformIO $ getLine
    in play O b i

runner :: (RandomGen g) => Board -> g -> Turn -> GameState
runner b g t =
    case getState (traceShow b b) of
        Open -> case t of
                    XTurn -> let (b', g') = hostPlay b g
                            in runner b' g' OTurn
                    OTurn -> runner (clientPlay b) g XTurn
        x -> x

main = print $ runner (replicate 9 Free) (mkStdGen 3) XTurn 

