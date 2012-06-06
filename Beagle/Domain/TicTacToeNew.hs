
-- TODO(jhibberd) Throw in lots of functions that could be useful and let the
-- natural selection process pick the ones it finds most useful.

module Beagle.Domain
    ( Gene(..)
    , genotypeLength
    , randomSeed
    , score
    ) where

import Beagle.Eval
import qualified Beagle.Log as Log
import qualified Data.Map as Map
import Debug.Trace
import System.IO.Unsafe
import System.Random

genotypeLength =        700
randomSeed =            60     :: Int

{-

    Grid Positions:
        A B C
        D E F
        G H I

    Game States:
        X, O, and N (none)

    Pattern States:
        X/X -       match X
        O/O -       match O
        T/taken -   match X, O
        F/free -    match N
        A/any -     match X, O, N

-}

data Gene = PlayA
          | PlayB
          | PlayC
          | PlayD
          | PlayE
          | PlayF
          | PlayG
          | PlayH
          | PlayI
          | If
          | Unless
          | End
          | Reset
          | SetAX
          | SetAO
          | SetAT
          | SetAF
          | SetAA
          | SetBX
          | SetBO
          | SetBT
          | SetBF
          | SetBA
          | SetCX
          | SetCO
          | SetCT
          | SetCF
          | SetCA
          | SetDX
          | SetDO
          | SetDT
          | SetDF
          | SetDA
          | SetEX
          | SetEO
          | SetET
          | SetEF
          | SetEA
          | SetFX
          | SetFO
          | SetFT
          | SetFF
          | SetFA
          | SetGX
          | SetGO
          | SetGT
          | SetGF
          | SetGA
          | SetHX
          | SetHO
          | SetHT
          | SetHF
          | SetHA
          | SetIX
          | SetIO
          | SetIT
          | SetIF
          | SetIA
          deriving (Ord, Eq, Show, Enum)

gmap :: Map.Map Gene ([Gene] -> Int -> State -> ([Gene], Int, State))
gmap = Map.fromList [
    (PlayA,     playA),
    (PlayB,     playB),
    (PlayC,     playC),
    (PlayD,     playD),
    (PlayE,     playE),
    (PlayF,     playF),
    (PlayG,     playG),
    (PlayH,     playH),
    (PlayI,     playI),
    (If,        flowIf),
    (Unless,    flowUnless),
    (End,       flowEnd),
    (Reset,     reset),
    (SetAX,     setAX), 
    (SetAO,     setAO),
    (SetAT,     setAT),
    (SetAF,     setAF),
    (SetAA,     setAA),
    (SetBX,     setBX),
    (SetBO,     setBO),
    (SetBT,     setBT),
    (SetBF,     setBF),
    (SetBA,     setBA),
    (SetCX,     setCX),
    (SetCO,     setCO),
    (SetCT,     setCT),
    (SetCF,     setCF),
    (SetCA,     setCA),
    (SetDX,     setDX),
    (SetDO,     setDO),
    (SetDT,     setDT),
    (SetDF,     setDF),
    (SetDA,     setDA),
    (SetEX,     setEX),
    (SetEO,     setEO),
    (SetET,     setET),
    (SetEF,     setEF),
    (SetEA,     setEA),
    (SetFX,     setFX),
    (SetFO,     setFO),
    (SetFT,     setFT),
    (SetFF,     setFF),
    (SetFA,     setFA),
    (SetGX,     setGX),
    (SetGO,     setGO),
    (SetGT,     setGT),
    (SetGF,     setGF),
    (SetGA,     setGA),
    (SetHX,     setHX),
    (SetHO,     setHO),
    (SetHT,     setHT),
    (SetHF,     setHF),
    (SetHA,     setHA),
    (SetIX,     setIX),
    (SetIO,     setIO),
    (SetIT,     setIT),
    (SetIF,     setIF),
    (SetIA,     setIA)
    ]

type Pos = Int
data Mark = N | X | O deriving (Eq, Show)
type Grid = [Mark]
data GameState = Draw | XWins | OWins | Open deriving (Show)
data Turn = XTurn | OTurn   

data PatternCellState = MatchX | MatchO | MatchT | MatchF | MatchA deriving (Show)
type Pattern = [PatternCellState]
type State = (Pattern, Grid)

-- | Gene functions ============================================================

-- | Functions for making a move -----------------------------------------------

playA, playB, playC, playD, playE, playF, playG, playH, playI 
    :: [Gene] -> Int -> State -> ([Gene], Int, State)

play :: Mark -> Pos -> [Gene] -> Int -> State -> ([Gene], Int, State)
play m p gs gi (ptn, grid) 
    | grid !! p /= N = (gs, gi+1, (ptn, grid)) -- skip move
    | otherwise = let (x,_:xs) = splitAt p grid 
                  in (gs, length gs, (ptn, x++(m:xs)))

playA = play O 0
playB = play O 1
playC = play O 2
playD = play O 3
playE = play O 4
playF = play O 5
playG = play O 6
playH = play O 7
playI = play O 8

-- | Functions for manipulating the current grid pattern -----------------------

setAX, setAO, setAT, setAF, setAA, setBX, setBO, setBT, setBF, setBA,
    setCX, setCO, setCT, setCF, setCA, setDX, setDO, setDT, setDF, setDA,
    setEX, setEO, setET, setEF, setEA, setFX, setFO, setFT, setFF, setFA,
    setGX, setGO, setGT, setGF, setGA, setHX, setHO, setHT, setHF, setHA,
    setIX, setIO, setIT, setIF, setIA
    :: [Gene] -> Int -> State -> ([Gene], Int, State)

reset :: [Gene] -> Int -> State -> ([Gene], Int, State)
reset gs gi (ptn, grid) = (gs, gi+1, (replicate 9 MatchA, grid))

set :: Int -> PatternCellState -> [Gene] -> Int -> State -> ([Gene], Int, State)
set pos state gs gi (ptn, grid) = let (x,_:xs) = splitAt pos ptn
                                  in (gs, gi+1, (x++(state:xs), grid))

setAX = set 0 MatchX
setAO = set 0 MatchO
setAT = set 0 MatchT
setAF = set 0 MatchF
setAA = set 0 MatchA
setBX = set 1 MatchX
setBO = set 1 MatchO
setBT = set 1 MatchT
setBF = set 1 MatchF
setBA = set 1 MatchA
setCX = set 2 MatchX
setCO = set 2 MatchO
setCT = set 2 MatchT
setCF = set 2 MatchF
setCA = set 2 MatchA
setDX = set 3 MatchX
setDO = set 3 MatchO
setDT = set 3 MatchT
setDF = set 3 MatchF
setDA = set 3 MatchA
setEX = set 4 MatchX
setEO = set 4 MatchO
setET = set 4 MatchT
setEF = set 4 MatchF
setEA = set 4 MatchA
setFX = set 5 MatchX
setFO = set 5 MatchO
setFT = set 5 MatchT
setFF = set 5 MatchF
setFA = set 5 MatchA
setGX = set 6 MatchX
setGO = set 6 MatchO
setGT = set 6 MatchT
setGF = set 6 MatchF
setGA = set 6 MatchA
setHX = set 7 MatchX
setHO = set 7 MatchO
setHT = set 7 MatchT
setHF = set 7 MatchF
setHA = set 7 MatchA
setIX = set 8 MatchX
setIO = set 8 MatchO
setIT = set 8 MatchT
setIF = set 8 MatchF
setIA = set 8 MatchA

-- | Functions for controlling program flow ------------------------------------

flowIf, flowUnless, flowEnd
    :: [Gene] -> Int -> State -> ([Gene], Int, State)

flowIf gs gi state@(ptn, grid) = case match ptn grid of
                                     True -> (gs, gi+1, state)
                                     False -> skipToEnd 1 gs (gi+1) state

flowUnless gs gi state@(ptn, grid) = case match ptn grid of
                                         True -> skipToEnd 1 gs (gi+1) state
                                         False -> (gs, gi+1, state)

match :: Pattern -> Grid -> Bool
match ptn grid = and . map match' $ zip ptn grid
    where match' :: (PatternCellState, Mark) -> Bool
          match' (p, m) = case p of
                           MatchX -> m == X
                           MatchO -> m == O
                           MatchT -> m == X || m == O
                           MatchF -> m == N
                           MatchA -> True

skipToEnd :: Int -> [Gene] -> Int -> State -> ([Gene], Int, State)
skipToEnd indent gs gi s
    | indent == 0 || gi >= (length gs -1) = (gs, gi, s)
    | otherwise = case gs !! gi of
                      If ->     skipToEnd (indent+1) gs (gi+1) s
                      Unless -> skipToEnd (indent+1) gs (gi+1) s
                      End ->    skipToEnd (indent-1) gs (gi+1) s
                      _ ->      skipToEnd indent gs (gi+1) s

flowEnd gs gi s = (gs, gi+1, s)

-- | Fitness function mechanics ================================================

getState :: Grid -> GameState
getState grid
    | doesXWin grid =               XWins
    | doesXWin (reverse' grid) =    OWins
    | filter (==N) grid == [] =     Draw
    | otherwise =                   Open
   
toState :: Grid -> State
toState grid = (replicate 9 MatchA, grid)

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
playGame gs (_, grid) g nt t =
    case getState grid of
        Open -> case t of

                    XTurn -> do
                        let (b', g') = hostPlay grid g
                        Log.msg gs ("H " ++ show grid ++ " -> " ++ show b')
                        playGame gs (toState b') g' (nt+1) OTurn

                    OTurn -> do
                        (ptn', grid') <- eval gs gmap 0 (toState grid)
                        if didMoveCorrectly grid' (nt+1)
                            then do
                                Log.msg gs ("F " ++ show grid ++ " -> " ++ show ptn' ++ show grid')
                                playGame gs (toState grid') g (nt+1) XTurn
                            else do 
                                Log.msg gs ("BAD_MOVE at " ++ show nt)
                                return (25-nt, g)

        XWins ->    do { Log.msg gs "- X_WIN"; return (10, g) }
        OWins ->    do { Log.msg gs "+ O_WIN"; return (0, g) }
        Draw ->     do { Log.msg gs "+ DRAW"; return (0, g) }

didMoveCorrectly :: Grid -> Int -> Bool
didMoveCorrectly grid numTurns = (length $ filter (/=N) grid) == numTurns 

-- | Manual testing of the runner ----------------------------------------------

{-
main = do
    s <- score [SetAF, If, SetBF, If, PlayB, End, PlayA, End, Reset, PlayC]
    print s
-}
