{-# LANGUAGE BangPatterns #-}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Prelude hiding (flip)
import System.Random

type Scenario = [Int] -- 9-dimensional vector representing game scenario
type Player = Int -- 1 or (-1)

-- | If position 'i' in the scenario is free (0) then place the player's mark
-- (1 or (-1)) and return the new scenario, otherwise return Nothing.
--
-- The player is detected from the marks already present in the scenario. See
-- 'detectTurn'.
maybePlay :: Scenario -> Int -> Maybe Scenario
maybePlay s i
    | s !! i == 0 = Just (play s i)
    | otherwise = Nothing

-- | Place the player's mark (1 or (-1)) at position 'i' in the scenario.
--
-- The player is detected from the marks already present in the scenario. See
-- 'detectTurn'.
play :: Scenario -> Int -> Scenario
play s i = let (x, _:xs) = splitAt i s in x ++ (detectTurn s:xs)

-- | Return whether a scenario represents a game that ended in a draw.
isDraw :: Scenario -> Bool
isDraw g = filter (==0) g == []

-- | Return whether a scenario represents a game that ended by player X (1) 
-- winning.
isWinner :: Scenario -> Bool
isWinner [1, 1, 1, _, _, _, _, _, _] = True
isWinner [_, _, _, 1, 1, 1, _, _, _] = True
isWinner [_, _, _, _, _, _, 1, 1, 1] = True
isWinner [1, _, _, 1, _, _, 1, _, _] = True
isWinner [_, 1, _, _, 1, _, _, 1, _] = True
isWinner [_, _, 1, _, _, 1, _, _, 1] = True
isWinner [1, _, _, _, 1, _, _, _, 1] = True
isWinner [_, _, 1, _, 1, _, 1, _, _] = True
isWinner _ = False

-- | Replace all player X (1) marks with player O (-1) marks. Useful when using
-- 'isWinner'.
invertPlayers :: Scenario -> Scenario
invertPlayers = map (\x -> case x of 1 -> (-1); (-1) -> 1; x -> x)

-- | Given a scenario return the player who needs to make the next move: 1 or 
-- -1.
--
-- This can be determined easily by counting the number of 1s and -1s present
-- in the scenario. 1 always goes first.
detectTurn :: Scenario -> Player
detectTurn s = case (numX == numO) of True -> 1; False -> (-1)
    where numX = count 1
          numO = count (-1)
          count x = length $ filter (==x) s

-- | Given a list of scenarios, transform each scenario to its base case and 
-- return a list of unique base cases.
toBases :: [Scenario] -> [Scenario]
toBases = nub . map (fst . toBase)

-- | Transform a single scenario to its base case.
--
-- A 'restore' function is also returned to transform the base case back to 
-- its original form.
toBase :: Scenario -> (Scenario, Scenario -> Scenario)
toBase = getBase . variants

-- | Given a list of 8 symmetrical scenarios, pick the base case (the one with 
-- the lowest lexicographical ordering.
--
-- Lexicographical order proved more efficient than hashing each scenario and 
-- picking the one with the lowest hash - and is sufficient; we just need an
-- arbitrary but deterministic member of the varitions to act as the base.
getBase :: Ord a => [(a, b)] -> (a, b)
getBase = head . sortBy (\(a, x) (b, y) -> compare a b) 

-- | Return whether a scenario represents a completed game (ie. there has been
-- a win or draw).
hasScenarioEnded :: Scenario -> Bool
hasScenarioEnded s 
    | isWinner s =                  True
    | isWinner $ invertPlayers s =  True
    | isDraw s =                    True
    | otherwise =                   False

-- | Given a scenario return all 8 symmetrical variations along with a function
-- that can be applied to any of the variations to restore the original 
-- scenario.
--
-- This 'restore' function is used to transform a base scenario to its original
-- form after a mark has been placed.
variants :: Scenario -> [(Scenario, Scenario -> Scenario)]
variants s = map (\(f, rev) -> (f s, rev)) fs
    where fs = [
            (id,                                id),
            (rotate,                            rotate. rotate. rotate),
            (rotate. rotate,                    rotate. rotate),
            (rotate. rotate. rotate,            rotate),
            (flip,                              flip),
            (rotate. flip,                      flip. rotate. rotate. rotate),
            (rotate. rotate. flip,              flip. rotate. rotate),
            (rotate. rotate. rotate. flip,      flip. rotate)
            ]

-- | Rotate a scenario 90 degrees clockwise.
-- 
-- a b c      g d a
-- d e f  ->  h e b
-- g h i      i f c
rotate :: Scenario -> Scenario
rotate (a:b:c:d:e:f:g:h:i:[]) = [g, d, a, h, e, b, i, f, c]

-- | Flip a scenario over its horizontal axis.
--
-- a b c      g h i
-- d e f  ->  d e f
-- g h i      a b c
flip :: Scenario -> Scenario
flip (a:b:c:d:e:f:g:h:i:[]) = [g, h, i, d, e, f, a, b, c]

-- | Given a scenario return all the subsequent scenarios that can be generated
-- by placing a single player mark.
immediateScenarios :: Scenario -> [Scenario]
immediateScenarios g
    | hasScenarioEnded g = []
    | otherwise = filter (not . hasScenarioEnded) . toBases . map fromJust . 
                  filter (/=Nothing) $ map (maybePlay g) [0..8]

-- | Given a scenario return all possible subsequent scenarios that can be 
-- generated by playing the scenario through to the end of a game, for every
-- possible sequence of moves.
subsequentScenarios :: Scenario -> [Scenario]
subsequentScenarios s = let is = immediateScenarios s
                            ss = nub . concat $ map subsequentScenarios is
                        in s:ss

--main = print . length $ subsequentScenarios [0, 0, 0, 0, 0, 0, 0, 0, 0]

-- HASH TABLE ------------------------------------------------------------------

type Alleles = [Int]

-- | Express a scenario as an int, for use as a hash table key.
toKey :: Scenario -> Int
toKey g = foldr f 0 (zip [0..] (allPositive g))
    where f (i, x) b = b + (x * (10 ^ i))
          allPositive = map (\x -> case x of (-1) -> 1; 0 -> 2; 1 -> 3)

-- | Build a table of gene index positions, keyed by base scenarios.
buildTable :: Map.Map Int Int
buildTable = Map.fromList $ zip scenarioKeys [0..] 
    where scenarioKeys = map toKey . subsequentScenarios $ replicate 9 0

-- | Return the gene index associated with a base scenario.
scenarioIndex :: Scenario -> Int
scenarioIndex s = case Map.lookup (toKey s) buildTable of
                      (Just i) -> i
                      Nothing ->  error "Illegal scenario"

-- | Using a list of alleles (genetically evolved) solve a scenario by placing
-- the optimum next move.
--
-- First convert the scenario to its base case (and restore function), then
-- lookup the optimum move for the base case in the alleles list, apply the
-- optimum move then transform the modified base case to its original state
-- (using the restore function).
solve :: Alleles -> Scenario -> Scenario
solve al scn = let (base, restore) = toBase scn
                   idx = scenarioIndex base
                   solution = al !! idx
                   scn' = play scn solution
                   scn'' = restore scn'
               in scn''

{-
mockAlleles = replicate 627 3

main = do
    let solve' = solve mockAlleles
    print $ solve' [1, 0, 0, 0, 0, 0, (-1), 0, 0]
-}

-- RUNNER ----------------------------------------------------------------------

populationSize = 2

-- | Create a list of randomly chosen alleles.
mkAlleles :: RandomGen g => g -> (Alleles, g)
mkAlleles = mkAlleles' (Map.size buildTable)
    where mkAlleles' 0 g = ([], g)
          mkAlleles' n g = let (x, g') = randomR (0, 8) g
                               (xs, g'') = mkAlleles' (n-1) g'
                           in (x:xs, g'')

-- | Generation a population of random alleles.
mkPopulation :: RandomGen g => g -> ([Alleles], g)
mkPopulation !g = mkPopulation' populationSize g
    where mkPopulation' 0 !g = ([], g)
          mkPopulation' n !g = let (x, g') = mkAlleles g
                                   (xs, g'') = mkPopulation' (n-1) g'
                               in (x:xs, g'') 

--main = print $ mkPopulation (mkStdGen 0)

-- FITNESS EVALUATION ----------------------------------------------------------

numGames = 100 -- TODO(jhibberd) Replace with system that plays all possible games.


-- TODO(jhibberd) Clean up
hostPlay :: RandomGen g => Scenario -> g -> (Scenario, g)
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


-- TODO Need bool indicating which mark the genome is playing as.
playGame :: (RandomGen g) 
         => (Scenario -> Scenario) -- bot 
         -> Scenario 
         -> g
         -> IO (Bool, g)
playGame gnm s g
    | isWinner s =                  return (True, g) -- TODO(jhibberd) Player X changes from bot to host!
    | isWinner $ invertPlayers s =  return (False, g) -- See above
    | isDraw s =                    return (True, g)
    | otherwise = 
          case (detectPlayer s) of

              1 -> do
                  let (s', g') = hostPlay s g
                  playGame gnm s' g'

              (-1) -> do
                  let s' = gnm s
                  if isLegalMove s s'
                      then playGame gnm s' g
                      else return (False, g) -- illegal move, so lose the game

-- | Return whether a change from a 'before' scenario to an 'after' scenario
-- represents a legal move.
isLegalMove :: Scenario -> Scenario -> Bool
isLegalMove before after = --TODO (length $ filter (/=N) grid) == numTurns 

