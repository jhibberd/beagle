-- | Genetic Algorithm

import qualified Beagle.Domain as D
import Beagle.Eval
import Beagle.Evolve
import qualified Beagle.Random as R
import Beagle.Stat
import Beagle.Type
import Control.Monad.State
import Data.List (sortBy)
import System.Random

-- | Create a new genotype consisting of randomly chosen genes.
mkgenotype :: RandomGen g => g -> (Genotype, g)
mkgenotype = f D.genotypeLength
    where f 0 g = ([], g)
          f n g = let (x, g') = R.gene g
                      (xs, g'') = f (n-1) g'
                  in (x:xs, g'')

-- | Calculate the delta (deviation) between the target phenotype and observed 
-- phenotype. The deviation is a float between zero and infinity: zero meaning
-- the candidate phenotype matched the target phenotype; infinity meaning that
-- the candidate couldn't be evaluated and therefore had no observable 
-- phenotype.
getDelta :: (Genotype, Maybe Phenotype)
         -> (Genotype, Maybe Phenotype, Delta)
getDelta (gt, pt) = (gt, pt, d pt)
    where d (Just pt') = abs (read pt' - D.targetPhenotype)
          d Nothing    = 1/0 -- Infinity

evalPopulation :: Population 
               -> State Counters [(Genotype, Maybe Phenotype, Delta)]
evalPopulation p = do
        modify . incrGenotypes . length $ p
        return . sort . map (getDelta . eval) $ p
    where sort = sortBy (\a b -> compare (delta a) (delta b))

-- | Generate a list (population) of genotypes consisting of randomly chosen
-- genes. The size of the population is fixed and determined by the domain.
popSeed :: RandomGen g => g -> (Population, g)
popSeed = f D.populationSize
    where f 0 g = ([], g)
          f n g = let (p, g') =   mkgenotype g
                      (ps, g'') = f (n-1) g'
                  in (p:ps, g'')

-- | Filter an evaluated population to return only candidates whose phenotype
-- matches the target phenotype.
solutions :: [(Genotype, Maybe Phenotype, Delta)] -> State Counters [Genotype]
solutions p = do
    let s = map (genotype) . filter (\gt -> delta gt == 0) $ p
    modify . incrSolutions $ length s
    return s

-- | Find n genotypes that exhibit target phenotypes, given a seed population
-- (p) and random generator (g).
solve :: RandomGen g 
      => Population 
      -> g
      -> Int
      -> State Counters [Genotype]
solve _ _ 0 = return []
solve p g n = do
    ep <- evalPopulation p
    let (nxp, g') = evolve g ep
    s <- solutions ep
    s' <- solve nxp g' (n - length s)
    return (s ++ s')

main = let (p, g') = popSeed R.g
       in print $ runState (solve p g' D.numSolutions) newCounters

