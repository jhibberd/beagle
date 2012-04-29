-- | Genetic Algorithm

import qualified Beagle.Domain as D
import Beagle.Eval2
import Beagle.Evolve
import qualified Beagle.Random as R
import Beagle.Stat
import Beagle.Type
import Control.Monad.State
import Data.List (sortBy, genericLength)
import System.Random

import Debug.Trace
import qualified Data.Map as Map

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
popScore :: [Genotype] -> [(Genotype, Score)]
popScore = sort . map (\x -> (x, D.score x))
    where sort = sortBy (\a b -> compare (snd a) (snd b))

-- | Generate a list (population) of genotypes consisting of randomly chosen
-- genes. The size of the population is fixed and determined by the domain.
popSeed :: RandomGen g => g -> (Population, g)
popSeed = f D.populationSize
    where f 0 g = ([], g)
          f n g = let (p, g') =   mkgenotype g
                      (ps, g'') = f (n-1) g'
                  in (p:ps, g'')

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
stalePop (a, n) = n > 70 && a < 50

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
{-
solve :: RandomGen g 
      => Population 
      -> g
      -> Bool -- Use intelligent evolution function (or random).
      -> State Counters Genotype
solve p g intel = do
    ep <- evalPopulation p
    case (solutions ep) of
        (Just x) -> return x
        Nothing -> 
            let evolveF = case intel of
                    True -> evolve
                    False -> evolveR
                (nxp, g') = evolveF g ep
            in solve nxp g' intel
-}
solve :: RandomGen g 
      => Population 
      -> g
      -> [(Score, Int)]
      -> Genotype
solve !p !g !hist =
    let ep = popScore p
    in case (solutions ep) of
        (Just x) -> x
        Nothing -> 
            let s = modeAvg (map snd ep) Map.empty
                hist' = addToHist s hist
                ep' = traceShow s ep
                (p', g') = case isLocalOptima hist' of
                    True -> popSeed g
                    False -> evolve g ep'
            in solve p' g' hist'

-- TODO(jhibberd) Might be worth trying to reintroduce stats as state, provided
-- that the algorithm still runs in constant space.
{-
main = do 
    let (p, g') = popSeed R.g
    print "Intelligent evolution:"
    let (sp, s) = runState (solve p g' True) newCounters 
    print $ (sp, s)
-}
main = do 
    let (p, g') = popSeed R.g
    print $ solve p g' []
