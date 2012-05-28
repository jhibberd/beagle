-- | Intelligently evolve an evaluated population to produce a new population of
-- genotypes.

module Beagle.Evolve
    ( evolve
    , tournamentSelection -- private
    , crossover -- private
    ) where

import qualified Beagle.Domain as D
import qualified Beagle.Log as Log
import qualified Beagle.Random as R
import Data.List
import System.Random

type Genotype = [D.Gene]
type Population = [Genotype]
type Score = Float

-- | Given a population of genotypes and their observed phenotypes, generate a
-- new population (generation) that will *probably* or *logically* perform 
-- better (at exhibiting the target phenotype).
evolve :: RandomGen g
       => g
       -> [(Genotype, Score)]
       -> IO (Population, g)
evolve g ps = f D.populationSize g
    where f 0 g = return ([], g)
          f n g = do
              (!x, g') <- make g
              (xs, g'') <- f (n-1) g'
              return (x:xs, g'')
          make g = do
              let (parentA, g') =   tournamentSelection ps 10 g
                  (parentB, g'') =  tournamentSelection ps 10 g'
                  (child, g''') =   crossover parentA parentB 30 g''
                  (child', g'''') = mutate child g'''
              -- # let (a:b:[], g') = pair g -- pick pair to breed
                  -- #(c, g'') = breed a b g'
                  -- mutate genes to avoid unhealthy gene pool convergence
                  -- # (c', g''') = (mutate c g'')
              Log.evolve parentA parentB child'
              return (child', g'''')
          -- # mset = multiset . nub . map fst $ ps
          -- #pair g
          -- #    | length mset > 1 = R.pick mset 2 g
          -- #    | otherwise = (mset++mset, g)

-- | Pick an individual from a population by first picking a sample of size 'n'
-- then selecting the individual from the sample with the best fitness.
-- 
-- If the tournament size is large, weak individuals have a smaller chance of
-- being selected.
tournamentSelection :: RandomGen g => [(a, Float)] -> Int -> g -> (a, g)
tournamentSelection ps n g = let (sample, g') = R.pick ps n g
                                 p = fst . head $ sortSnd sample
                             in (p, g')

sortSnd :: Ord b => [(a, b)] -> [(a, b)]
sortSnd = sortBy (\a b -> compare (snd a) (snd b))


-- | Mutate n randomly chosen genes of a genotype to prevent the population
-- from iteratively converging around a small subset of all available genes.
--
-- A 1% rate of mutation appears to work well. Too small and the population
-- converges on a small gene pool; too large and the benefits of crossover are
-- lost to random mutations.
mutate :: RandomGen g => Genotype -> g -> (Genotype, g)
mutate gt g = R.map (\_ g -> R.gene g) gt numMutations g
    where numMutations = ceiling $ (fromInteger D.genotypeLength) * mutationRate
          mutationRate = 0.01


-- | TODO(jhibberd) Pick n random points in the chromosome, for the child use
-- all contiguous genes up to random point 1 from parent A the all contiguous
-- blocks from parentB up to point 2 etc. Which parent starts should be random

crossover :: RandomGen g => [a] -> [a] -> Int -> g -> ([a], g)
crossover pa pb n g = let (startParent, g') = getStartParent
                          (pts, g'') = points g'
                          child = crossover' pa pb 0 pts startParent
                    in (child, g'')
    where points g = let (sample, g') = R.pick [0 .. (length pa -1)] n g
                     in (sort sample, g')
          getStartParent = randomR (False, True) g

crossover' :: [a] -> [a] -> Int -> [Int] -> Bool -> [a]
crossover' pa pb i is flag
    | i == length pa = []
    | otherwise = let c =            parallelGet pa pb i flag
                      (flag', is') = maybeSwitchFlag flag i is
                  in c : crossover' pa pb (i+1) is' flag'

maybeSwitchFlag :: Bool -> Int -> [Int] -> (Bool, [Int])
maybeSwitchFlag flag _ [] = (flag, [])
maybeSwitchFlag flag i is@(i':is')
    | i == i' = (not flag, is')
    | otherwise = (flag, is)

parallelGet :: [a] -> [a] -> Int -> Bool -> a
parallelGet as bs i flag
    | flag == True = as !! i
    | flag == False = bs !! i

