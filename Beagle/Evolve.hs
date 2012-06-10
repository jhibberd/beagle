-- | Intelligently evolve an evaluated population to produce a new population of
-- genotypes.

module Beagle.Evolve
    ( evolve
    , tournamentSelection -- private
    , crossover -- private
    , mutate -- private
    , replicate -- private
    , mutate' -- private
    , crossover' -- private
    ) where

import qualified Beagle.Domain as D
import qualified Beagle.Log as Log
import qualified Beagle.Random as R
import Beagle.Eval
import Data.List hiding (replicate)
import System.Random
import Prelude hiding (replicate)


-- | CONFIG --------------------------------------------------------------------

tournamentSize = 30
pMutates = 0.01
pCrossover = 0.05

type Genotype = [D.Gene]
type Population = [Genotype]
type Score = Float
type Genome = [D.Gene]

-- | Given a population of genotypes and their observed phenotypes, generate a
-- new population (generation) that will *probably* or *logically* perform 
-- better (at exhibiting the target phenotype).
evolve :: RandomGen g
       => g
       -> [(Genotype, Score)]
       -> Int
       -> IO (Population, g)
evolve g ps popSize = f popSize g
    where f 0 g = return ([], g)
          f n g = do
              (!x, g') <- make g
              (xs, g'') <- f (n-1) g'
              return (x:xs, g'')
          make g = do
              {-
              let (parentA, scoreA, g') =   tournamentSelection ps 30 g
                  (parentB, scoreB, g'') =  tournamentSelection ps 30 g'
                  (child, g''') =   crossover parentA parentB 1 g''
                  (child', g'''') = mutate child g'''
                  -- TODO(jhibberd) Get best parent score
                  (bestParent, bestScore) = if scoreA <= scoreB 
                                                then (parentA, scoreA)
                                                else (parentB, scoreB)
              -- TODO(jhibberd) eval the child.
              s <- D.score child'
              -- TODO(jhibberd) If child worse than best parent, drop child
              -- and return best parent
              let (xx, gg) = if s > bestScore 
                        then changeParent bestParent g''''
                        else (child', g'''')
              -- TODO(jhibberd) Maybe mutate the parent?
              -- # let (a:b:[], g') = pair g -- pick pair to breed
                  -- #(c, g'') = breed a b g'
                  -- mutate genes to avoid unhealthy gene pool convergence
                  -- # (c', g''') = (mutate c g'')
              Log.evolve parentA parentB child'
              return (xx, gg)
              -}
              return ([], g)

{-
changeParent :: RandomGen g => [D.Gene] -> g -> ([D.Gene], g)
changeParent xs g = let (n, g') = randomR (1, 10) g
                    in f n xs g'
    where f :: RandomGen g => Int -> Genotype -> g -> (Genotype, g)
          f n 
            | n >= 8 = reproduce
            | n >= 6 = rotate
            | otherwise = mutate
-}

-- | GENETIC OPERATORS ---------------------------------------------------------

-- | Replicate an individual (unchanged) to the next generation.
replicate :: RandomGen g => [([a], Float)] -> g -> ([a], g)
replicate = tournamentSelection

-- | TODO Mutate n randomly chosen genes of a genotype to prevent the population
-- from iteratively converging around a small subset of all available genes.
mutate :: RandomGen g => [(Genome, Float)] -> g -> (Genome, g)
mutate pop g = let (individual, g') = tournamentSelection pop g
               in mutate' individual g' pMutates R.gene

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
                           in case focus of
                                True -> (fst x, (g', focus'))
                                False -> (snd x, (g', focus'))

-- | GENETIC OPERATOR UTILITIES ------------------------------------------------

-- | Pick an individual from a population by first picking a sample of size 'n'
-- then selecting the individual from the sample with the best fitness.
-- 
-- If the tournament size is large, weak individuals have a smaller chance of
-- being selected.
tournamentSelection :: RandomGen g => [([a], Float)] -> g -> ([a], g)
tournamentSelection ps g = let (sample, g') = R.pick ps tournamentSize g
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

mapS :: [a] -> (a -> s -> (b, s)) -> s -> ([b], s)
mapS [] _ s = ([], s)
mapS (x:xs) f s = let (x', s') =    f x s
                      (xs', s'') =  mapS xs f s'
                  in (x':xs', s'')

