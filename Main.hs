-- | Genetic Algorithm

import qualified Beagle.Domain as D
import Beagle.Evolve
import qualified Beagle.Log as Log
import qualified Beagle.Random as R
--import Beagle.Type
import Data.List (sortBy, genericLength)
import System.Random

import Debug.Trace
import qualified Data.Map as Map

type Genotype = [D.Gene]
type Population = [Genotype]
type Score = Float

-- | Create a new genotype consisting of 'Empty' genes. 
--
-- The genetic algorithm works better when starting with 'Empty' genes rather
-- than starting with a sequence of randomly selected genes.
blankGenotype :: [D.Gene]
blankGenotype = replicate (fromIntegral D.genotypeLength) D.Empty

-- | Using the domain's fitness function (D.score), calculate the score (a value
-- between 0 and 1) of each genotype in the population. The resultant list of
-- genotype/score pairs is sorted by score.
popScore :: [Genotype] -> IO [(Genotype, Score)]
popScore = fmap sort . sequence . map score
    where sort = sortBy (\a b -> compare (snd a) (snd b))
          score x = do
              s <- D.score x
              Log.score x s
              return (x, s)

-- | Generate a list (population) of genotypes consisting of randomly chosen
-- genes. The size of the population is fixed and determined by the domain.
popSeed :: Population
popSeed = replicate D.populationSize blankGenotype

-- Experimental

modeAvg :: (Ord a) => [a] -> Map.Map a Int -> (a, Int)
modeAvg [] m = head . reverse . sortBy (\a b -> compare (snd a) (snd b)) $ Map.toList m
modeAvg (x:xs) m = modeAvg xs (Map.insert x (getFreq x +1) m)
    where getFreq k = case Map.lookup k m of
                        Nothing -> 0
                        (Just x) -> x


addToHist :: (a, Int) -> [(a, Int)] -> [(a, Int)]
addToHist x xs = take 20 (x:xs)

isLocalOptima :: [(Float, Int)] -> Bool
isLocalOptima xs
    | length xs < 10 = False
    | otherwise = case all stalePop xs of
                      True -> trace (show "LO! " ++ show xs) True
                      False -> False

stalePop :: (Float, Int) -> Bool
stalePop (a, n) = n > 70 && a < 0.99

-- END

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
      -> [(Score, Int)]
      -> IO Genotype
solve !p !g !gen !hist = do
    Log.generation gen p
    ep <- popScore p
    case (solutions ep) of
        (Just x) -> return x
        Nothing -> do
            let s = modeAvg (map snd ep) Map.empty
                hist' = addToHist s hist
                ep' = traceShow s ep
            (p', g') <- case isLocalOptima hist' of
                True -> return (popSeed, g)
                False -> evolve g ep'
            solve p' g' (gen+1) hist'

main = do 
    Log.setUp
    solve popSeed R.g 1 []

