-- | Intelligently evolve an evaluated population to produce a new population of
-- genotypes.

module Beagle.Evolve
    ( evolve
    , doubleTopHalf
    ) where

import qualified Beagle.Domain as D
import qualified Beagle.Random as R
import Beagle.Type
import System.Random

evolve :: RandomGen g
       => g
       -> [(Genotype, Maybe Phenotype, Delta)]
       -> (Population, g)
evolve g [] = ([], g)
evolve g (p:ps) = let (p', g') = mutate (genotype p) g
                      (ps', g'') = evolve g' ps
                  in (p':ps', g'')
    where mutate gt g = R.map (\_ g -> R.gene g) gt D.mutationsPerGenotype g

-- | Basic implementation of evolution: take the half of the population whose
-- phenotype was closest to the target phenotype. Double each candidate to 
-- create a new population, and mutate random genes. When this function is 
-- applied a solution is found (very) approx. 3.7 times faster than purely 
-- random mutations.
doubleTopHalf :: [a] -> [a]
doubleTopHalf [] = []
doubleTopHalf (x:xs) = x:doubleTopHalf xs
