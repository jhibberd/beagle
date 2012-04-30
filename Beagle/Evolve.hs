-- | Intelligently evolve an evaluated population to produce a new population of
-- genotypes.

module Beagle.Evolve
    ( evolve
    ) where

import qualified Beagle.Domain as D
import qualified Beagle.Log as L
import qualified Beagle.Random as R
import Beagle.Type
import Data.List
import System.Random

-- TODO(jhibberd) Devise tests that can be run overnight that evaluate an 
-- intelligent evolutionary algorithm compared with a random algorithm. Use
-- this to determine algorithm "progress".

-- TODO(jhibberd) Need unit tests and detailed logging so it's possible to see
-- the evolutionary path of genotypes.

-- | Given a population of genotypes and their observed phenotypes, generate a
-- new population (generation) that will *probably* or *logically* perform 
-- better (at exhibiting the target phenotype).
evolve :: RandomGen g
       => g
       -> [(Genotype, Score)]
       -> (Population, g)
evolve g ps = f D.populationSize g
    where f 0 g = ([], g)
          f n g = let (!x, g') = make g
                      (xs, g'') = f (n-1) g'
                  in (x:xs, g'')
          make g = let (a:b:[], g') = pair g -- pick pair to breed
                       (x, g'') = breed a b g'
                 in mutate x g'' -- mutate n genes to maintain variance
          mset = multiset . nub . map fst $ ps
          pair g
              | length mset > 1 = R.pick mset 2 g
              | otherwise = (mset++mset, g)

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

-- | Breed two genotypes by creating a new genotype consisting of randonly
-- selected genes from both parents. An equal number of genes will be picked 
-- from each parent. The gene ordinal positions within the genotype do not
-- change.
--
-- [x, x, x, x, x, x]
-- [y, y, y, y, y, y]
-- =>
-- [x, y, x, x, y, y]
-- 
breed :: RandomGen g => Genotype -> Genotype -> g -> (Genotype, g)
breed a b g = let (c, g') = f (zip a b) g
              in (L.breed a b c c, g')
    where f [] g = ([], g)
          f ((a, b):xs) g = let (switch, g') = randomR (True, False) g
                                (xs', g'') = f xs g'
                                !x' = case switch of
                                    True -> a
                                    False -> b
                            in (x':xs', g'')   

-- | Return multiset of population genotypes where the genotype multiplicity
-- indicates the genotype "success" (in exhibiting the target phenotype). The
-- multiset will be used to pick pairs to breed; the higher the multiplicity
-- the higher the probability of being picked for breeding.
--
-- TODO(jhibberd) Weighting might be better there than linear.
multiset :: Population -> [Genotype]
multiset = f . zip [1..] . reverse 
    where f [] = []
          f ((i, x):xs) = replicate i x ++ f xs

