-- | Genetic Algorithm

import qualified Beagle.Domain as D
import Beagle.Eval
import Beagle.Evolve
import qualified Beagle.Random as R
import Beagle.Type
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

evalPopulation :: Population -> [(Genotype, Maybe Phenotype, Delta)]
evalPopulation = sort . map (getDelta . eval)
    where sort = sortBy (\a b -> compare (delta a) (delta b))

-- | Generate a list (population) of genotypes consisting of randomly chosen
-- genes. The size of the population is fixed and determined by the domain.
popSeed :: RandomGen g => g -> (Population, g)
popSeed = f D.populationSize
    where f 0 g = ([], g)
          f n g = let (p, g') =   mkgenotype g
                      (ps, g'') = f (n-1) g'
                  in (p:ps, g'')

solutions :: a -- ^ tag (eg stats object)
          -> [(Genotype, Maybe Phenotype, Delta)]
          -> [(Genotype, a)]
solutions a = map (addtag . genotype) . filter (\(_, _, d) -> d == 0)
    where addtag x = (x, a)

solve :: RandomGen g 
      => Population 
      -> g
      -> Int
      -> [(Genotype, Int)]
solve [] _ _ = []
solve p g i = let ep = evalPopulation p
                  (nxp, g') = evolve g ep
            in solutions i ep ++ solve nxp g' (i+D.populationSize)

main = let (p, g') = popSeed R.g
       in print . take 1 $ solve p g' 0

