{-# LANGUAGE BangPatterns #-}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Prelude hiding (flip)
import System.Random

import Debug.Trace

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
                   scn' = play base solution
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

{- NEXT... (then evolve)

-- | Using the domain's fitness function (D.score), calculate the score (a value
-- between 0 and 1) of each genotype in the population. The resultant list of
-- genotype/score pairs is sorted by score.
popScore :: [Genotype] -> IO [(Genotype, Score)]
popScore gt = fmap sort . sequence $ map score gt
    where sort = sortBy (\a b -> compare (snd a) (snd b))
          score x = do
              s <- D.score x
              Log.score x s
              return (x, s)

-- | Given a list of genotypes and their evaluated scores, return the first
-- genotype whose score is 0 (exhibits target phenotype), or Nothing if none
-- can be found.
solutions :: [(Genotype, Score)] -> Maybe Genotype
solutions p =
    case (map fst . filter (\gt -> snd gt == 0) $ p) of
        [] -> Nothing
        (x:xs) -> Just x

-- Given a seed population, intelligently evolve the population until a member
-- exhibits the target phenotype.
solve :: RandomGen g 
      => Population 
      -> g
      -> Int -- Generation
      -> IO Genotype
solve !p !g !gen = do
    Log.generation gen p
    ep <- popScore p
    case (solutions ep) of
        (Just x) -> return x
        Nothing -> do
            let s = modeAvg (map snd ep) Map.empty
                ep' = traceShow s ep
            (p', g') <- evolve g ep' popSize 
            solve p' g' (gen+1)

main = do 
    Log.setUp
    let (p, g') = popSeed R.g
    g <- solve p g' 1
    print g
-}
--main = print $ mkPopulation (mkStdGen 0)

-- FITNESS EVALUATION ----------------------------------------------------------

numGames = 40 -- TODO(jhibberd) Replace with system that plays all possible games.

-- | Given a list of outcomes (success/failure) calculate and return a score.
score :: [Bool] -> Float
score xs =  (realToFrac total - realToFrac lost) / realToFrac total
    where total = length xs
          lost = length $ filter (==False) xs

-- | Play n games and return a fitness score based on the aggregated outcomes.
evalFitness :: RandomGen g 
            => (Scenario -> Scenario) -- genome function 
            -> g 
            -> IO Float
evalFitness gnm g = do 
        outcomes <- evalFitness' gnm numGames g
        return (score outcomes)
    where evalFitness' gnm 0 _ = return []
          evalFitness' gnm n g = do
              (x, g') <- playGame gnm emptyScenario g (getPlayer n)
              xs <- evalFitness' gnm (n-1) g'
              return (x:xs)
          getPlayer n = case (odd n) of True -> 1; False -> (-1)
          emptyScenario = replicate 9 0

-- Play a single game of Tic-Tac-Toe: genome function vs. random playing host.
playGame :: (RandomGen g) 
         => (Scenario -> Scenario) -- genome function 
         -> Scenario -- current game scenario 
         -> g
         -> Int -- player being used by genome: 1 or (-1)
         -> IO (Bool, g)
playGame gnm s g gp
    | isWinner s =                  return (gp == 1, g)
    | isWinner $ invertPlayers s =  return (gp == (-1), g)
    | isDraw s =                    return (True, g)
    | otherwise = 
          case (detectTurn s == gp) of

              True -> do
                  let s' = gnm s
                  if isLegalMove s (traceShow s' s')
                      then playGame gnm s' g gp
                      else return (False, traceShow "boo" g) -- illegal move, so lose the game

              False -> do
                  let (s', g') = hostPlay s g
                  playGame gnm (traceShow s' s') g' gp

-- | Return whether a change from a 'before' scenario to an 'after' scenario
-- represents a legal move.
isLegalMove :: Scenario -> Scenario -> Bool
isLegalMove before after = 
        case (detectTurn before) of
            1 ->    numX after == numX before +1 && numO after == numO before
            (-1) -> numO after == numO before +1 && numX after == numX before
    where numX = count 1
          numO = count (-1)
          count x = length . filter (==x)

-- | Simulate an opponent by playing a random legal move.
-- TODO Soon to be replaced by an algorithm that plays every possible game.
hostPlay :: RandomGen g => Scenario -> g -> (Scenario, g)
hostPlay s g = let (i, g') = randomR (0, length freeonly -1) g
                   i'' = freeonly !! i
                   s' = play s i''
                in (s', g')
    where zipped = zip s [0..]
          freeonly = map snd $ filter ((==0) . fst) zipped

main = do
    let (alleles, g') = mkAlleles (mkStdGen 1)
        gnm = solve alleles
    evalFitness gnm g'

--main = return $ score [False, False, True, False]

