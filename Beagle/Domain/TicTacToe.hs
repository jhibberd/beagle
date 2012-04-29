
-- TODO(jhibberd) Maybe, if all problems could be modelled as functions that
-- take varying numbers of state as arguments and return state, the algorithm
-- would be easier to develop?

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
    , genemap
    , genotypeLength
    , mutationsPerGenotype
    , populationSize
    , randomSeed
    , runner
    ) where

import Data.Dynamic
import qualified Data.Map as Map
import Debug.Trace
import Data.Typeable
import System.IO.Unsafe
import System.Random

instance Typeable Mark where
    typeOf _ = mkTyConApp (mkTyCon3 "Beagle" "Domain" "Mark") []

genotypeLength =        50     :: Int
mutationsPerGenotype =  2      :: Int
populationSize =        100    :: Int
randomSeed =            6      :: Int

data Gene = PlayA | PlayB | PlayC | PlayD | PlayE | PlayF | PlayG | PlayH
          | PlayI | IsAX | IsBX | IsCX | IsDX | IsEX | IsFX | IsGX | IsHX 
          | IsIX | IsAO | IsBO | IsCO | IsDO | IsEO | IsFO | IsGO | IsHO
          | IsIO | IsAN | IsBN | IsCN | IsDN | IsEN | IsFN | IsGN | IsHN
          | IsIN | Branch | Or | And | DoNothing | Empty
          deriving (Ord, Eq, Show, Enum)

genemap :: Map.Map Gene (Dynamic, Int)
genemap = Map.fromList [
    (PlayA,         (toDyn playA,       1)),
    (PlayB,         (toDyn playB,       1)),
    (PlayC,         (toDyn playC,       1)),
    (PlayD,         (toDyn playD,       1)),
    (PlayE,         (toDyn playE,       1)),
    (PlayF,         (toDyn playF,       1)),
    (PlayG,         (toDyn playG,       1)),
    (PlayH,         (toDyn playH,       1)),
    (PlayI,         (toDyn playI,       1)),
    (IsAX,          (toDyn isAX,        1)),
    (IsBX,          (toDyn isBX,        1)),
    (IsCX,          (toDyn isCX,        1)),
    (IsDX,          (toDyn isDX,        1)),
    (IsEX,          (toDyn isEX,        1)),
    (IsFX,          (toDyn isFX,        1)),
    (IsGX,          (toDyn isGX,        1)),
    (IsHX,          (toDyn isHX,        1)),
    (IsIX,          (toDyn isIX,        1)),
    (IsAO,          (toDyn isAO,        1)),
    (IsBO,          (toDyn isBO,        1)),
    (IsCO,          (toDyn isCO,        1)),
    (IsDO,          (toDyn isDO,        1)),
    (IsEO,          (toDyn isEO,        1)),
    (IsFO,          (toDyn isFO,        1)),
    (IsGO,          (toDyn isGO,        1)),
    (IsHO,          (toDyn isHO,        1)),
    (IsIO,          (toDyn isIO,        1)),
    (IsAN,          (toDyn isAN,        1)),
    (IsBN,          (toDyn isBN,        1)),
    (IsCN,          (toDyn isCN,        1)),
    (IsDN,          (toDyn isDN,        1)),
    (IsEN,          (toDyn isEN,        1)),
    (IsFN,          (toDyn isFN,        1)),
    (IsGN,          (toDyn isGN,        1)),
    (IsHN,          (toDyn isHN,        1)),
    (IsIN,          (toDyn isIN,        1)),
    (Branch,        (toDyn branch,      3)),
    (Or,            (toDyn or',         2)),
    (And,           (toDyn and',        2)),
    (DoNothing,     (toDyn doNothing,   1))
    ]

runner =            (toDyn runner',     1 :: Int)

type Pos = Int
data Mark = N | X | O deriving (Eq, Show)
type Grid = [Mark]
data GameState = Draw | XWins | OWins | Open deriving (Show)
data Turn = XTurn | OTurn
type State = (Bool, Grid)

-- | Gene functions ============================================================

-- | Functions for making a move -----------------------------------------------

playA, playB, playC, playD, playE, playF, playG, playH, playI :: State -> State

play :: Mark -> Pos -> State -> State
play m p (_, grid) 
    | grid !! p /= N = toState grid -- skip move
    | otherwise = let (x,_:xs) = splitAt p grid 
                  in toState (x++(m:xs))

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

isAX, isBX, isCX, isDX, isEX, isFX, isGX, isHX, isIX :: State -> State 
isAO, isBO, isCO, isDO, isEO, isFO, isGO, isHO, isIO :: State -> State
isAN, isBN, isCN, isDN, isEN, isFN, isGN, isHN, isIN :: State -> State

is :: Pos -> Mark -> State -> State
is p m (_, grid) = (grid !! p == m, grid)

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

branch :: State -> State -> State -> State
branch (True, _) x _ = x
branch (False, _) _ x = x

or' :: State -> State -> State
or' (True, x) _ = (True, x)
or' _ (True, x) = (True, x)
or' (False, x) _ = (False, x)

and' :: State -> State -> State
and' (True, x) (True, _) = (True, x)
and' (_, x) _ = (False, x)

doNothing :: State -> State
doNothing = id

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
                  in (fromState $ play X i'' (toState grid), g')
    where zipped = zip grid [0..]
          freeonly = map snd $ filter ((==N) . fst) zipped

toScore :: Int -> Float
toScore = (/10) . fromIntegral

numLoses :: [Bool] -> Int
numLoses = length . filter (==False)

runner' :: (State -> State) -> Float
runner' f = toScore . numLoses $ runner'' f numGames g OTurn
    where g = mkStdGen 6
          numGames = 3

runner'' :: (RandomGen g) 
         => (State -> State) 
         -> Int 
         -> g 
         -> Turn 
         -> [Bool]
runner'' _ 0 _ _ = []
runner'' f n g t = let (outcome, g') = playGame f newGrid g t 
                   in outcome: runner'' f (n-1) g' nextT
    where newGrid = toState (replicate 9 N)
          nextT = case t of
                      XTurn -> OTurn
                      OTurn -> XTurn

-- TODO(jhibberd) Should probably be win=3, draw=1, loss=0
playGame :: (RandomGen g) 
         => (State -> State) 
         -> State 
         -> g 
         -> Turn 
         -> (Bool, g)
playGame f (_, grid) g t =
    case getState grid {-(traceShow grid grid)-} of
        Open -> case t of
                    XTurn -> let (b', g') = hostPlay grid g
                             in playGame f (toState b') g' OTurn
                    OTurn -> playGame f (f $ toState grid) g XTurn
        XWins -> (False, g)
        OWins -> (True, g)
        Draw -> (False, g)

-- | Manual testing of the runner ----------------------------------------------

-- | Mock genotype that allows the user to manually control how the genotype
-- behaves.
mockF :: State -> State
mockF (_, grid) = let i = read .unsafePerformIO $ getLine
                  in play O i (toState grid)

main = print $ runner' mockF 

