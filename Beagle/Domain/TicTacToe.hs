
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

genotypeLength =        30
populationSize =        10     :: Int
randomSeed =            6      :: Int


data Gene = PlayA | PlayB | PlayC | PlayD | PlayE | PlayF | PlayG | PlayH
          | PlayI | IsAX | IsBX | IsCX | IsDX | IsEX | IsFX | IsGX | IsHX 
          | IsIX | IsAO | IsBO | IsCO | IsDO | IsEO | IsFO | IsGO | IsHO
          | IsIO | IsAN | IsBN | IsCN | IsDN | IsEN | IsFN | IsGN | IsHN
          | IsIN | FlagTrue | FlagFalse | FlagAnd | FlagOr | FlagSwitch | Empty


          | APlayA | APlayB | APlayC | APlayD | APlayE | APlayF | APlayG | APlayH
          | APlayI | AIsAX | AIsBX | AIsCX | AIsDX | AIsEX | AIsFX | AIsGX | AIsHX 
          | AIsIX | AIsAO | AIsBO | AIsCO | AIsDO | AIsEO | AIsFO | AIsGO | AIsHO
          | AIsIO | AIsAN | AIsBN | AIsCN | AIsDN | AIsEN | AIsFN | AIsGN | AIsHN
          | AIsIN | AFlagTrue | AFlagFalse | AFlagAnd | AFlagOr | AFlagSwitch | AEmpty

          | BPlayA | BPlayB | BPlayC | BPlayD | BPlayE | BPlayF | BPlayG | BPlayH
          | BPlayI | BIsAX | BIsBX | BIsCX | BIsDX | BIsEX | BIsFX | BIsGX | BIsHX 
          | BIsIX | BIsAO | BIsBO | BIsCO | BIsDO | BIsEO | BIsFO | BIsGO | BIsHO
          | BIsIO | BIsAN | BIsBN | BIsCN | BIsDN | BIsEN | BIsFN | BIsGN | BIsHN
          | BIsIN | BFlagTrue | BFlagFalse | BFlagAnd | BFlagOr | BFlagSwitch | BEmpty

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
    (FlagTrue,      flagTrue),
    (FlagFalse,     flagFalse),
    (FlagAnd,       flagAnd),
    (FlagOr,        flagOr),
    (FlagSwitch,    flagSwitch),
    (Empty,         empty),

    (APlayA,         putA PlayA),
    (APlayB,         putA PlayB),
    (APlayC,         putA PlayC),
    (APlayD,         putA PlayD),
    (APlayE,         putA PlayE),
    (APlayF,         putA PlayF),
    (APlayG,         putA PlayG),
    (APlayH,         putA PlayH),
    (APlayI,         putA PlayI),
    (AIsAX,          putA IsAX),
    (AIsBX,          putA IsBX),
    (AIsCX,          putA IsCX),
    (AIsDX,          putA IsDX),
    (AIsEX,          putA IsEX),
    (AIsFX,          putA IsFX),
    (AIsGX,          putA IsGX),
    (AIsHX,          putA IsHX),
    (AIsIX,          putA IsIX),
    (AIsAO,          putA IsAO),
    (AIsBO,          putA IsBO),
    (AIsCO,          putA IsCO),
    (AIsDO,          putA IsDO),
    (AIsEO,          putA IsEO),
    (AIsFO,          putA IsFO),
    (AIsGO,          putA IsGO),
    (AIsHO,          putA IsHO),
    (AIsIO,          putA IsIO),
    (AIsAN,          putA IsAN),
    (AIsBN,          putA IsBN),
    (AIsCN,          putA IsCN),
    (AIsDN,          putA IsDN),
    (AIsEN,          putA IsEN),
    (AIsFN,          putA IsFN),
    (AIsGN,          putA IsGN),
    (AIsHN,          putA IsHN),
    (AIsIN,          putA IsIN),
    (AFlagTrue,      putA FlagTrue),
    (AFlagFalse,     putA FlagFalse),
    (AFlagAnd,       putA FlagAnd),
    (AFlagOr,        putA FlagOr),
    (AFlagSwitch,    putA FlagSwitch),
    (AEmpty,         putA Empty),

    (BPlayA,         putB PlayA),
    (BPlayB,         putB PlayB),
    (BPlayC,         putB PlayC),
    (BPlayD,         putB PlayD),
    (BPlayE,         putB PlayE),
    (BPlayF,         putB PlayF),
    (BPlayG,         putB PlayG),
    (BPlayH,         putB PlayH),
    (BPlayI,         putB PlayI),
    (BIsAX,          putB IsAX),
    (BIsBX,          putB IsBX),
    (BIsCX,          putB IsCX),
    (BIsDX,          putB IsDX),
    (BIsEX,          putB IsEX),
    (BIsFX,          putB IsFX),
    (BIsGX,          putB IsGX),
    (BIsHX,          putB IsHX),
    (BIsIX,          putB IsIX),
    (BIsAO,          putB IsAO),
    (BIsBO,          putB IsBO),
    (BIsCO,          putB IsCO),
    (BIsDO,          putB IsDO),
    (BIsEO,          putB IsEO),
    (BIsFO,          putB IsFO),
    (BIsGO,          putB IsGO),
    (BIsHO,          putB IsHO),
    (BIsIO,          putB IsIO),
    (BIsAN,          putB IsAN),
    (BIsBN,          putB IsBN),
    (BIsCN,          putB IsCN),
    (BIsDN,          putB IsDN),
    (BIsEN,          putB IsEN),
    (BIsFN,          putB IsFN),
    (BIsGN,          putB IsGN),
    (BIsHN,          putB IsHN),
    (BIsIN,          putB IsIN),
    (BFlagTrue,      putB FlagTrue),
    (BFlagFalse,     putB FlagFalse),
    (BFlagAnd,       putB FlagAnd),
    (BFlagOr,        putB FlagOr),
    (BFlagSwitch,    putB FlagSwitch),
    (BEmpty,         putB Empty)
    ]

type Pos = Int
data Mark = N | X | O deriving (Eq, Show)
type Grid = [Mark]
data GameState = Draw | XWins | OWins | Open deriving (Show)
data Turn = XTurn | OTurn
type State = 
    ( Bool      -- Flag A
    , Bool      -- Flag B
    , Bool      -- Which flag is active
    , [Gene]    -- Branch A
    , [Gene]    -- Branch B
    , Grid
    )

-- | Gene functions ============================================================

-- | Functions for making a move -----------------------------------------------

playA, playB, playC, playD, playE, playF, playG, playH, playI 
    :: [Gene] -> Int -> State -> ([Gene], Int, State)

-- TODO(jhibberd) All it takes is one of these for a position that is already
-- taken and that essentially junks the genotype. Instead try skipping to the
-- next gene if the position is already take - like 'empty'.
play :: Mark -> Pos -> [Gene] -> Int -> State -> ([Gene], Int, State)
play m p gs gi (fa, fb, fs, ba, bb, grid) 
    | grid !! p /= N = (gs, length gs, (fa, fb, fs, ba, bb, grid)) -- skip move
    | otherwise = let (x,_:xs) = splitAt p grid 
                  in (gs, length gs, (fa, fb, fs, ba, bb, x++(m:xs)))

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
is p m gs gi (fa, fb, True, ba, bb, grid) = (gs, gi+1, (value, fb, True, ba, bb, grid))
    where value = grid !! p == m
is p m gs gi (fa, fb, False, ba, bb, grid) = (gs, gi+1, (fa, value, False, ba, bb, grid))
    where value = grid !! p == m

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

putA, putB :: Gene -> [Gene] -> Int -> State -> ([Gene], Int, State)
putA g gs gi (fa, fb, fs, ba, bb, grid) = (gs, gi+1, (fa, fb, fs, ba++[g], bb, grid))
putB g gs gi (fa, fb, fs, ba, bb, grid) = (gs, gi+1, (fa, fb, fs, ba, bb++[g], grid))

flagAnd, flagOr, flagTrue, flagFalse, empty, flagSwitch, doBranchA, doBranchB
    :: [Gene] -> Int -> State -> ([Gene], Int, State)

flagAnd gs gi (True, True, fs, ba, bb, grid) = doBranchA gs gi (True, True, fs, ba, bb, grid)
flagAnd gs gi (fa, fb, fs, ba, bb, grid) = doBranchB gs gi (fa, fb, fs, ba, bb, grid)

flagOr gs gi (True, fb, fs, ba, bb, grid) = doBranchA gs gi (True, fb, fs, ba, bb, grid)
flagOr gs gi (fa, True, fs, ba, bb, grid) = doBranchA gs gi (fa, True, fs, ba, bb, grid)
flagOr gs gi (fa, fb, fs, ba, bb, grid) = doBranchB gs gi (fa, fb, fs, ba, bb, grid)

flagTrue gs gi (True, fb, fs, ba, bb, grid) = doBranchA gs gi (True, fb, fs, ba, bb, grid)
flagTrue gs gi (fa, fb, fs, ba, bb, grid) = doBranchB gs gi (fa, fb, fs, ba, bb, grid)

flagFalse gs gi (False, fb, fs, ba, bb, grid) = doBranchA gs gi (False, fb, fs, ba, bb, grid)
flagFalse gs gi (fa, fb, fs, ba, bb, grid) = doBranchB gs gi (fa, fb, fs, ba, bb, grid)

doBranchA gs gi (fa, fb, fs, ba, bb, grid) = (gs', 0, (fa, fb, fs, [], [], grid))
    where gs' = ba ++ (drop (gi+1) gs)

doBranchB gs gi (fa, fb, fs, ba, bb, grid) = (gs', 0, (fa, fb, fs, [], [], grid))
    where gs' = bb ++ (drop (gi+1) gs)

flagSwitch gs gi (fa, fb, fs, ba, bb, grid) = (gs, gi+1, (fa, fb, not fs, ba, bb, grid)) 
empty gs gi s = (gs, gi+1, s)

-- | Fitness function mechanics ================================================

getState :: Grid -> GameState
getState grid
    | doesXWin grid =               XWins
    | doesXWin (reverse' grid) =    OWins
    | filter (==N) grid == [] =     Draw
    | otherwise =                   Open
   
toState :: Grid -> State
toState grid = (False, False, False, [], [], grid)

fromState :: State -> Grid
fromState (_, _, _, _, _, grid) = grid

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
                      (_, _, (_, _, _, _, _, grid')) = play X i'' [] 0 (toState grid)
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
          numGames = 2

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
playGame gs (_, _, _, _, _, grid) g nt t =
    case getState (traceShow grid grid) of
        Open -> case t of
                    XTurn -> let (b', g') = hostPlay grid g
                             in playGame gs (toState b') g' (nt+1) OTurn
                    OTurn -> let s = eval gs gmap 0 $ toState grid
                             in case didMoveCorrectly s (nt+1) of
                                    False -> (25-nt, g)
                                    True -> playGame gs s g (nt+1) XTurn
        XWins -> (10, g)
        OWins -> (0, g)
        Draw -> (5, g)

didMoveCorrectly :: State -> Int -> Bool
didMoveCorrectly (_, _, _, _, _, grid) numTurns = (length $ filter (/=N) grid) == numTurns 

-- | Manual testing of the runner ----------------------------------------------

main = print $ score [APlayA, FlagSwitch, IsAN, FlagTrue, PlayB]

