-- | Genetic Algorithm

import qualified Beagle.Domain as D
import Beagle.Evolve
import qualified Beagle.Log as Log
import qualified Beagle.Random as R
import Data.List (sortBy, genericLength)
import System.Random

import Debug.Trace
import qualified Data.Map as Map

type Genotype = [D.Gene]
type Population = [Genotype]
type Score = Float

-- | Define population size. According to research this should be as big as the
-- memory in the execution environment will allow. Certainly >500.
popSize :: Int
popSize = 1000

-- | Create a new genotype consisting of randomly chosen genes.
mkgenotype :: RandomGen g => g -> (Genotype, g)
mkgenotype = f D.genotypeLength
    where f 0 g = ([], g)
          f n g = let (x, g') = R.gene g
                      (xs, g'') = f (n-1) g'
                  in (x:xs, g'')

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

-- | Generate a list (population) of genotypes consisting of randomly chosen
-- genes. The size of the population is fixed and determined by the domain.
popSeed :: RandomGen g => g -> (Population, g)
popSeed !g = f 0 g []
    where f :: RandomGen g => Int -> g -> [[D.Gene]] -> (Population, g)
          f n !g xs 
              | n == popSize = (xs, g)
              | otherwise = let (x, g') = mkgenotype g
                            in f (n+1) g' (x:xs)

-- Debug
modeAvg :: (Ord a) => [a] -> Map.Map a Int -> (a, Int)
modeAvg [] m = head . reverse . sortBy (\a b -> compare (snd a) (snd b)) $ Map.toList m
modeAvg (x:xs) m = modeAvg xs (Map.insert x (getFreq x +1) m)
    where getFreq k = case Map.lookup k m of
                        Nothing -> 0
                        (Just x) -> x

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

