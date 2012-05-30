
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
import qualified Beagle.Log as Log
import qualified Data.Map as Map
import Debug.Trace
import System.IO.Unsafe
import System.Random

genotypeLength =        500
populationSize =        300    :: Int
randomSeed =            12     :: Int


data Gene = PlayA | PlayB | PlayC | PlayD | PlayE | PlayF | PlayG | PlayH
          | PlayI | IsAX | IsBX | IsCX | IsDX | IsEX | IsFX | IsGX | IsHX 
          | IsIX | IsAO | IsBO | IsCO | IsDO | IsEO | IsFO | IsGO | IsHO
          | IsIO | IsAN | IsBN | IsCN | IsDN | IsEN | IsFN | IsGN | IsHN
          | IsIN | JumpUnlessFlagTrue | JumpUnlessFlagFalse | JumpUnlessFlagAnd 
          | JumpUnlessFlagOr | JumpHere | FlagSwitch | Empty
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
    (JumpUnlessFlagTrue,        jumpUnlessFlagTrue),
    (JumpUnlessFlagFalse,       jumpUnlessFlagFalse),
    (JumpUnlessFlagAnd,         jumpUnlessFlagAnd),
    (JumpUnlessFlagOr,          jumpUnlessFlagOr),
    (JumpHere,                  jumpHere),  
    (FlagSwitch,    flagSwitch),
    (Empty,         empty)
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
    | grid !! p /= N = (gs, gi+1, (fa, fb, fs, ba, bb, grid)) -- skip move
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

jumpUnlessFlagAnd, jumpUnlessFlagOr, jumpUnlessFlagTrue, jumpUnlessFlagFalse, empty, flagSwitch, jumpHere
    :: [Gene] -> Int -> State -> ([Gene], Int, State)

jumpHere gs gi s = (gs, gi+1, s)
empty gs gi s = (gs, gi+1, s)
flagSwitch gs gi (fa, fb, fs, ba, bb, grid) = (gs, gi+1, (fa, fb, not fs, ba, bb, grid)) 

jumpUnlessFlagAnd gs gi (True, True, fs, ba, bb, grid) = (gs, gi+1, (True, True, fs, ba, bb, grid))
jumpUnlessFlagAnd gs gi s = jump 1 gs (gi+1) s 

jumpUnlessFlagOr gs gi (True, fb, fs, ba, bb, grid) = (gs, gi+1, (True, fb, fs, ba, bb, grid))
jumpUnlessFlagOr gs gi (fa, True, fs, ba, bb, grid) = (gs, gi+1, (fa, True, fs, ba, bb, grid))
jumpUnlessFlagOr gs gi s = jump 1 gs (gi+1) s 

jumpUnlessFlagTrue gs gi (True, fb, fs, ba, bb, grid) = (gs, gi+1, (True, fb, fs, ba, bb, grid))
jumpUnlessFlagTrue gs gi s = jump 1 gs (gi+1) s 

jumpUnlessFlagFalse gs gi (False, fb, fs, ba, bb, grid) = (gs, gi+1, (False, fb, fs, ba, bb, grid))
jumpUnlessFlagFalse gs gi s = jump 1 gs (gi+1) s 

jump :: Int -> [Gene] -> Int -> State -> ([Gene], Int, State)
jump count gs gi s
    | count == 0 || gi >= (length gs -1) = (gs, gi, s)
    | otherwise = case gs !! gi of
                    JumpUnlessFlagAnd -> jump (count+1) gs (gi+1) s
                    JumpUnlessFlagOr -> jump (count+1) gs (gi+1) s
                    JumpUnlessFlagTrue -> jump (count+1) gs (gi+1) s
                    JumpUnlessFlagFalse -> jump (count+1) gs (gi+1) s
                    JumpHere -> jump (count-1) gs (gi+1) s
                    _ -> jump count gs (gi+1) s

-- | Fitness function mechanics ================================================

getState :: Grid -> GameState
getState grid
    | doesXWin grid =               XWins
    | doesXWin (reverse' grid) =    OWins
    | filter (==N) grid == [] =     Draw
    | otherwise =                   Open
   
toState :: Grid -> State
toState grid = (False, False, True, [], [], grid)

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

score :: [Gene] -> IO Float
score gs = do
        realScore <- runner'' gs numGames g OTurn
        return $ toScore realScore
    where g = mkStdGen 12
          numGames = 100

runner'' :: (RandomGen g) 
         => [Gene] 
         -> Int 
         -> g 
         -> Turn 
         -> IO [Int]
runner'' _ 0 _ _ = return []
runner'' gs n g t = do
        (outcome, g') <- playGame gs newGrid g 0 t
        outcome' <- runner'' gs (n-1) g' nextT
        return (outcome:outcome')
    where newGrid = toState (replicate 9 N)
          nextT = case t of
                      XTurn -> OTurn
                      OTurn -> XTurn

playGame :: (RandomGen g) 
         => [Gene] 
         -> State 
         -> g
         -> Int -- Number of turns taken
         -> Turn 
         -> IO (Int, g)
playGame gs (_, _, _, _, _, grid) g nt t =
    case getState grid of
        Open -> case t of

                    XTurn -> do
                        let (b', g') = hostPlay grid g
                        Log.msg gs ("H " ++ show grid ++ " -> " ++ show b')
                        playGame gs (toState b') g' (nt+1) OTurn

                    OTurn -> do
                        s <- eval gs gmap 0 (toState grid)
                        if didMoveCorrectly s (nt+1)
                            then do
                                let s' = show $ fromState s
                                Log.msg gs ("F " ++ show grid ++ " -> " ++ s')
                                playGame gs s g (nt+1) XTurn
                            else do 
                                Log.msg gs ("BAD_MOVE at " ++ show nt)
                                return (25-nt, g)

        XWins ->    do { Log.msg gs "- X_WIN"; return (10, g) }
        OWins ->    do { Log.msg gs "+ O_WIN"; return (0, g) }
        Draw ->     do { Log.msg gs "+ DRAW"; return (0, g) }

didMoveCorrectly :: State -> Int -> Bool
didMoveCorrectly (_, _, _, _, _, grid) numTurns = (length $ filter (/=N) grid) == numTurns 

-- | Manual testing of the runner ----------------------------------------------

main = do
    s <- score [IsAN, JumpUnlessFlagFalse, IsBN, JumpUnlessFlagTrue, PlayB, JumpHere, PlayA, JumpHere, PlayD]
    print s

