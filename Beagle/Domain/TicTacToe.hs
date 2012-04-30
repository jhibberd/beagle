
-- TODO(jhibberd) Throw in lots of functions that could be useful and let the
-- natural selection process pick the ones it finds most useful.

{- Board positions are labelled as follows:
   
    A | B | C
   -----------
    D | E | F
   -----------
    G | H | I

-}

module Beagle.Domain
    ( Gene(..)
    , genotypeLength
    , populationSize
    , randomSeed
    , score
    ) where

import Beagle.Eval
import qualified Data.Map as Map
import Debug.Trace
import System.IO.Unsafe
import System.Random

genotypeLength =        20
populationSize =        10     :: Int
randomSeed =            6      :: Int

data Gene = PlayA | PlayB | PlayC | PlayD | PlayE | PlayF | PlayG | PlayH
          | PlayI | IsAX | IsBX | IsCX | IsDX | IsEX | IsFX | IsGX | IsHX 
          | IsIX | IsAO | IsBO | IsCO | IsDO | IsEO | IsFO | IsGO | IsHO
          | IsIO | IsAN | IsBN | IsCN | IsDN | IsEN | IsFN | IsGN | IsHN
          | IsIN | IfTrue | IfFalse | Empty | End
          deriving (Ord, Eq, Show, Enum)

gmap :: Map.Map Gene ([Gene] -> Int -> State -> ([Gene], Int, State))
gmap = Map.fromList [
    (PlayA,         playA),
    (PlayB,         playB),
    (PlayC,         playC),
    (PlayD,         playD),
    (PlayE,         playE),
    (PlayF,         playF),
    (PlayG,         playG),
    (PlayH,         playH),
    (PlayI,         playI),
    (IsAX,          isAX),
    (IsBX,          isBX),
    (IsCX,          isCX),
    (IsDX,          isDX),
    (IsEX,          isEX),
    (IsFX,          isFX),
    (IsGX,          isGX),
    (IsHX,          isHX),
    (IsIX,          isIX),
    (IsAO,          isAO),
    (IsBO,          isBO),
    (IsCO,          isCO),
    (IsDO,          isDO),
    (IsEO,          isEO),
    (IsFO,          isFO),
    (IsGO,          isGO),
    (IsHO,          isHO),
    (IsIO,          isIO),
    (IsAN,          isAN),
    (IsBN,          isBN),
    (IsCN,          isCN),
    (IsDN,          isDN),
    (IsEN,          isEN),
    (IsFN,          isFN),
    (IsGN,          isGN),
    (IsHN,          isHN),
    (IsIN,          isIN),
    (IfTrue,        ifTrue),
    (IfFalse,       ifFalse),
    (Empty,         empty),
    (End,           end)
    ]

type Pos = Int
data Mark = N | X | O deriving (Eq, Show)
type Grid = [Mark]
data GameState = Draw | XWins | OWins | Open deriving (Show)
data Turn = XTurn | OTurn
type State = (Bool, Grid)

-- | Gene functions ============================================================

-- | Functions for making a move -----------------------------------------------

playA, playB, playC, playD, playE, playF, playG, playH, playI 
    :: [Gene] -> Int -> State -> ([Gene], Int, State)

play :: Mark -> Pos -> [Gene] -> Int -> State -> ([Gene], Int, State)
play m p gs gi (_, grid) 
    | grid !! p /= N = (gs, gi+1, toState grid) -- skip move
    | otherwise = let (x,_:xs) = splitAt p grid 
                  in (gs, gi+1, toState (x++(m:xs)))

playA = play O 0
playB = play O 1
playC = play O 2
playD = play O 3
playE = play O 4
playF = play O 5
playG = play O 6
playH = play O 7
playI = play O 8

-- | Functions for determining the current board state -------------------------

isAX, isBX, isCX, isDX, isEX, isFX, isGX, isHX, isIX 
    :: [Gene] -> Int -> State -> ([Gene], Int, State)   
isAO, isBO, isCO, isDO, isEO, isFO, isGO, isHO, isIO 
    :: [Gene] -> Int -> State -> ([Gene], Int, State)
isAN, isBN, isCN, isDN, isEN, isFN, isGN, isHN, isIN 
    :: [Gene] -> Int -> State -> ([Gene], Int, State)

is :: Pos -> Mark -> [Gene] -> Int -> State -> ([Gene], Int, State)
is p m gs gi (_, grid) = (gs, gi+1, (grid !! p == m, grid))

isAX = is 0 X 
isBX = is 1 X 
isCX = is 2 X
isDX = is 3 X 
isEX = is 4 X 
isFX = is 5 X 
isGX = is 6 X 
isHX = is 7 X 
isIX = is 8 X 
isAO = is 0 O 
isBO = is 1 O 
isCO = is 2 O 
isDO = is 3 O 
isEO = is 4 O 
isFO = is 5 O 
isGO = is 6 O 
isHO = is 7 O 
isIO = is 8 O 
isAN = is 0 N
isBN = is 1 N
isCN = is 2 N
isDN = is 3 N
isEN = is 4 N
isFN = is 5 N
isGN = is 6 N
isHN = is 7 N
isIN = is 8 N

-- | Functions for controlling program flow ------------------------------------

-- TODO(jhibberd) Implement basic logic using single interface:
-- [Gene] -> Int -> State -> ([Gene], Int, State)
--
-- 

ifTrue, ifFalse, empty, end :: [Gene] -> Int -> State -> ([Gene], Int, State)

ifTrue gs gi (True, grid)   = (gs, gi+10, (True, grid))
ifTrue gs gi (False, grid)  = (gs, gi+1, (False, grid))

ifFalse gs gi (True, grid)  = (gs, gi+1, (True, grid))
ifFalse gs gi (False, grid) = (gs, gi+10, (False, grid))

empty gs gi s = (gs, gi+1, s)
end gs gi s = (gs, length gs, s)

-- | Fitness function mechanics ================================================

getState :: Grid -> GameState
getState grid
    | doesXWin grid =               XWins
    | doesXWin (reverse' grid) =    OWins
    | filter (==N) grid == [] =     Draw
    | otherwise =                   Open
   
toState :: Grid -> State
toState grid = (False, grid)

fromState :: State -> Grid
fromState (_, grid) = grid

reverse' :: Grid -> Grid
reverse' = map (\x -> case x of
                    X -> O
                    O -> X
                    x -> x)

doesXWin :: Grid -> Bool
doesXWin [X, X, X, _, _, _, _, _, _] = True
doesXWin [_, _, _, X, X, X, _, _, _] = True
doesXWin [_, _, _, _, _, _, X, X, X] = True
doesXWin [X, _, _, X, _, _, X, _, _] = True
doesXWin [_, X, _, _, X, _, _, X, _] = True
doesXWin [_, _, X, _, _, X, _, _, X] = True
doesXWin [X, _, _, _, X, _, _, _, X] = True
doesXWin [_, _, X, _, X, _, X, _, _] = True
doesXWin _ = False

hostPlay :: (RandomGen g) => Grid -> g -> (Grid, g)
hostPlay grid g = let (i, g') = randomR (0, (length freeonly)-1) g
                      i'' = freeonly !! i
                      (_, _, (_, grid')) = play X i'' [] 0 (toState grid)
                  in (grid', g')
    where zipped = zip grid [0..]
          freeonly = map snd $ filter ((==N) . fst) zipped

toScore :: [Int] -> Float
toScore xs = 1 - (1 / (fromIntegral $ (sum xs +1))) 

-- TODO(jhibberd) Cleanup (and abstract?) local optima work.
-- TODO(jhibberd) Calculate num. mutations per genotype at 5% rate
--                Time with and w/0 to ensure correct implementation.

score :: [Gene] -> Float
score gs = toScore $ runner'' gs numGames g OTurn
    where g = mkStdGen 6
          numGames = 1

runner'' :: (RandomGen g) 
         => [Gene] 
         -> Int 
         -> g 
         -> Turn 
         -> [Int]
runner'' _ 0 _ _ = []
runner'' gs n g t = let (outcome, g') = playGame gs newGrid g 0 t 
                    in outcome: runner'' gs (n-1) g' nextT
    where newGrid = toState (replicate 9 N)
          nextT = case t of
                      XTurn -> OTurn
                      OTurn -> XTurn

-- TODO(jhibberd) Should probably be win=3, draw=1, loss=0
playGame :: (RandomGen g) 
         => [Gene] 
         -> State 
         -> g
         -> Int -- Number of turns taken
         -> Turn 
         -> (Int, g)
playGame gs (_, grid) g nt t =
    case getState (traceShow grid grid) of
        Open -> case t of
                    XTurn -> let (b', g') = hostPlay grid g
                             in playGame gs (toState b') g' (nt+1) OTurn
                    OTurn -> let s = eval gs gmap 0 $ toState grid
                             in case didMoveCorrectly s (nt+1) of
                                    False -> (25-nt, g)
                                    True -> playGame gs s g (nt+1) XTurn
        XWins -> (0, g)
        OWins -> (10, g)
        Draw -> (5, g)

didMoveCorrectly :: State -> Int -> Bool
didMoveCorrectly (_, grid) numTurns = (length $ filter (/=N) grid) == numTurns 

-- | Manual testing of the runner ----------------------------------------------

main = print $ score [IsIX, IfTrue, PlayA]

