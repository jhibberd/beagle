-- | Intelligently evolve an evaluated population to produce a new population of
-- genotypes.

module Beagle.Evolve
    ( evolve
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
              let (a:b:[], g') = pair g -- pick pair to breed
                  (c, g'') = breed a b g'
                  -- mutate genes to avoid unhealthy gene pool convergence
                  (c', g''') = (mutate c g'')
              Log.evolve a b c'
              return (c', g''')
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
breed a b g = f (zip a b) g
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

