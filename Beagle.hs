{-# LANGUAGE BangPatterns #-}

module Beagle where

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Prelude hiding (flip)
import System.Random

populationSize =    500     -- The number of candidates in each generation.
tournamentSize =    10      -- The sample size to use when picking random
                            -- candidates for a genertic operation. The smaller
                            -- the size, the more tolerance for weaker 
                            -- candidates.
pMutates =          0.0001  -- During a mutation operation, the probability that
                            -- a given gene will be mutated.
pCrossover =        0.1     -- During a crossover operation, the probability
                            -- that a given gene will cause the dominant parent
                            -- to be switched.
pOpMutate =         0.05    -- The probability that the 'mutation' genetic
                            -- operator will be applied during the evolution
                            -- phase.
pOpReplica =        0.05    -- See above.
pOpCrossover =      0.9     -- See above.

type Scenario = [Int] -- 9-dimensional vector representing game scenario
type Player = Int -- 1 or (-1)

-- TODO: New play
play :: Scenario -> Int -> Scenario
play scn i = modifyScn (freeIdx !! idx)
    where scnIdx = zip scn [(0::Int)..]
          freeIdx = map snd $ filter (\x -> fst x == 0) scnIdx
          idx = i `mod` (length freeIdx)
          modifyScn i' = let (x, _:xs) = splitAt i' scn 
                         in x ++ (detectTurn scn:xs)

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
getBase = head . sortBy (\(a, _) (b, _) -> compare a b) 

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
immScenarios :: Scenario -> [Scenario]
immScenarios s
    | hasScenarioEnded s = []
    | otherwise = filter (not . hasScenarioEnded) $ allLegalMoves s 

-- | Given a scenario return all possible subsequent scenarios that can be 
-- generated by playing the scenario through to the end of a game, for every
-- possible sequence of moves.
subScenarios :: Scenario -> [Scenario]
subScenarios s = s : (nub . concat . map subScenarios $ immScenarios s)

-- HASH TABLE ------------------------------------------------------------------

type Alleles = [Int]

-- | Express a scenario as an int, for use as a hash table key.
toKey :: Scenario -> Int
toKey g = foldr f 0 (zip [0..] (allPositive g))
    where f (i, x) b = b + (x * (10 ^ i))
          allPositive = map (\x -> case x of (-1) -> 1; 0 -> 2; 1 -> 3)

-- | Convert a key to a scenario. Used by the player module.
fromKey :: Int -> Scenario
fromKey = reverse . map (\x -> case x of '1' -> (-1); '2' -> 0; '3' -> 1) . show

-- | Build a table of gene index positions, keyed by base scenarios.
buildTable :: Map.Map Int Int
buildTable = Map.fromList $ zip scenarioKeys [0..] 
    where scenarioKeys = map toKey . subScenarios $ replicate 9 0

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

-- RUNNER ----------------------------------------------------------------------

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

-- | Evaluate the fitness of each individual in the population.
--
-- Fitness is a float between 0 and 1, where 0 is perfect fitness.
scorePopulation :: [Alleles] -> IO [(Alleles, Float)]
scorePopulation = fmap sort . sequence . map score
    where sort = sortBy (\a b -> compare (snd a) (snd b))
          score x = do
              s <- evalFitness (solve x)
              return (x, s)

-- | Given a list of alleles and their fitnes scores, return the first genotype
-- whose score is 0 (exhibits target behaviour), or Nothing if none exist.
solutions :: [(Alleles, Float)] -> Maybe Alleles
solutions xs =
    case (map fst . filter (\x -> snd x == 0) $ xs) of
        [] ->     Nothing
        (x:xs) -> Just x

-- Given a seed population, intelligently evolve the population until an 
-- individual exhibits the target behaviour.
search :: RandomGen g 
       => [Alleles] 
       -> g
       -> Int -- generation
       -> IO Alleles
search !p !g !gen = do
    ep <- scorePopulation p
    putStrLn . show . minimum $ map snd ep -- debug
    case (solutions ep) of
        (Just x) -> return x
        Nothing -> do
            (p', g') <- evolve g ep 
            search p' g' (gen+1)

-- FITNESS EVALUATION ----------------------------------------------------------

-- | Given a list of outcomes (success/failure) calculate and return a score.
score :: [Bool] -> Float
score xs = (realToFrac total - realToFrac didntLose) / realToFrac total
    where total = length xs
          didntLose = length $ filter (==True) xs

-- | Play n games and return a fitness score based on the aggregated outcomes.
evalFitness :: (Scenario -> Scenario) -- genome function 
            -> IO Float
evalFitness gnm = do 
        outcomes <- evalFitness' gnm
        return (score outcomes)
    where evalFitness' gnm = do
              xFst <- playGame gnm 1 emptyScenario
              xSnd <- playGame gnm (-1) emptyScenario
              return (xFst++xSnd)
          emptyScenario = replicate 9 0

-- | Play an exhaustive list of all possible Tic-Tac-Toe games with a genome
-- and return a list of outcomes (True for win/draw, False for lose).
-- 
-- When it's the genome's turn, the genome will play what it believes is the
-- optimum move (given the scenario). When it's the hosts's turn, the host will
-- play every possible move.
playGame :: (Scenario -> Scenario)  -- genome function
         -> Int                     -- mark being used by genome: 1 or (-1)
         -> Scenario                -- current game scenario
         -> IO [Bool]               -- list of outcomes of all possible games
playGame gnm gp scn 
    | isWinner scn =                    return [gp == 1]
    | isWinner $ invertPlayers scn =    return [gp == (-1)]
    | isDraw scn =                      return [True]
    | otherwise =
        case (detectTurn scn == gp) of
            True ->  playGame gnm gp (gnm scn) -- genome play
            False -> fmap concat . sequence $ 
                     map (playGame gnm gp) (allLegalMoves scn) -- host play

-- | Return all (base) legal moves that can be made from a scenario.
allLegalMoves :: Scenario -> [Scenario]
allLegalMoves scn = toBases $ map (play scn) [0..8]

-- EVOLUTION -------------------------------------------------------------------

-- | Given a population of genotypes and their observed phenotypes, generate a
-- new population (generation) that will *probably* or *logically* perform 
-- better (at exhibiting the target phenotype).
evolve :: RandomGen g
       => g
       -> [(Alleles, Float)]
       -> IO ([Alleles], g)
evolve g xs = return $ evolve' xs g populationSize
    where populationSize = length xs

evolve' _ g 0 = ([], g)
evolve' xs g n = let (f, g') =      probdist dist g
                     (x, g'') =     f xs g'
                     (xs', g''') =  evolve' xs g'' (n-1)
                 in (x:xs', g''')
    where dist = [
              (mutate,      pOpMutate),
              (replica,     pOpReplica),
              (crossover,   pOpCrossover)
              ]

probdist :: RandomGen g => [(a, Float)] -> g -> (a, g)
probdist dist g = if (sum $ map snd dist) == 1.0
                      then let (r, g') = randomR (0.0, 1.0) g
                               cum = tail $ scanl (+) 0 (map snd dist)
                               dist' = zip (map fst dist) cum
                               outcome = probdist' dist' r
                           in (outcome, g')
                      else error "Incomplete probability distribution"

probdist' :: [(a, Float)] -> Float -> a
probdist' [] _ = error "Incomplete probability distribution"
probdist' ((el, p):xs) rp = if rp <= p then el else probdist' xs rp

-- | GENETIC OPERATORS ---------------------------------------------------------

-- | Replicate an individual (unchanged) to the next generation.
replica :: RandomGen g => [([a], Float)] -> g -> ([a], g)
replica = tournamentSelection

-- | TODO Mutate n randomly chosen genes of a genotype to prevent the population
-- from iteratively converging around a small subset of all available genes.
mutate :: RandomGen g => [(Alleles, Float)] -> g -> (Alleles, g)
mutate pop g = let (individual, g') = tournamentSelection pop g
               in mutate' individual g' pMutates (randomR (0, 8))

mutate' :: RandomGen g => [a] -> g -> Float -> (g -> (a, g)) -> ([a], g)
mutate' xs g p m = mapS xs f g
    where f x g = let (outcome, g') = prob p g
                  in case outcome of
                         True ->  m g'
                         False -> (x, g')

crossover :: RandomGen g => [([a], Float)] -> g -> ([a], g)
crossover pop g = let (parentA, g') =  tournamentSelection pop g
                      (parentB, g'') = tournamentSelection pop g'
                      (first, g''') =  randomR (False, True) g''
                  in crossover' (zip parentA parentB) g''' first pCrossover

crossover' :: RandomGen g => [(a, a)] -> g -> Bool -> Float -> ([a], g)
crossover' xs g focus p = let (xs', (g', _)) = mapS xs f (g, focus)
                          in (xs', g')
    where f x (g, focus) = let (switch, g') = prob p g 
                               focus' = case switch of
                                            True -> not focus
                                            False -> focus
                               !x' = case focus of True -> fst x; False -> snd x
                           in (x', (g', focus'))

-- | GENETIC OPERATOR UTILITIES ------------------------------------------------

-- | Pick an individual from a population by first picking a sample of size 'n'
-- then selecting the individual from the sample with the best fitness.
-- 
-- If the tournament size is large, weak individuals have a smaller chance of
-- being selected.
tournamentSelection :: RandomGen g => [([a], Float)] -> g -> ([a], g)
tournamentSelection ps g = let (sample, g') = choice ps tournamentSize g
                               p = fst . head $ sortSnd sample
                           in (p, g')

-- | Sort a list of tuples by its second element.
sortSnd :: Ord b => [(a, b)] -> [(a, b)]
sortSnd = sortBy (\a b -> compare (snd a) (snd b))

-- | Given an event probability and pseudo random number generator return 
-- whether the event "happened".
prob :: RandomGen g => Float -> g -> (Bool, g)
prob p g = let (x, g') = randomR (0.0, 1.0) g
           in ((x <= p), g')

-- | TODO
mapS :: [a] -> (a -> s -> (b, s)) -> s -> ([b], s)
mapS [] _ s = ([], s)
mapS (x:xs) f s = let (x', s') =    f x s
                      (xs', s'') =  mapS xs f s'
                  in (x':xs', s'')

-- | Return n random elements from a list.
choice :: RandomGen g => [a] -> Int -> g -> ([a], g)
choice _ 0 g = ([], g)
choice xs n g = let (ri, g') =   randomR range g
                    (xs', g'') = choice (lsdrop xs ri) (n-1) g'
                in ((xs !! ri):xs', g'')
    where range = (0, (length xs)-1)

-- | Remove element at a specific index position from a list.
lsdrop :: [a] -> Int -> [a]
lsdrop [] _ = []
lsdrop xs i = let (a, b) = splitAt i xs in a ++ (drop 1 b)

