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

{-
evalPopulation :: Population 
               -> State Counters [(Genotype, Score)]
evalPopulation p = do
        modify . incrGenotypes . length $ p
        return . sort . map eval $ p
    where sort = sortBy (\a b -> compare (snd a) (snd b))
-}
evalPopulation :: Population 
               -> [(Genotype, Score)]
evalPopulation p = sort . map eval $ p
    where sort = sortBy (\a b -> compare (snd a) (snd b))

-- | Generate a list (population) of genotypes consisting of randomly chosen
-- genes. The size of the population is fixed and determined by the domain.
popSeed :: RandomGen g => g -> (Population, g)
popSeed = f D.populationSize
    where f 0 g = ([], g)
          f n g = let (p, g') =   mkgenotype g
                      (ps, g'') = f (n-1) g'
                  in (p:ps, g'')

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
      -> Genotype
solve !p !g =
    let ep = evalPopulation p
    in case (solutions ep) of
        (Just x) -> x
        Nothing -> 
            let (p', g') = evolve g ep
            in solve p' g'

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
    print $ solve p g'

