-- | Genetic Algorithm

import Beagle.Type
import Beagle.Eval
import Data.List (sortBy)
import Data.Maybe (fromJust)
import System.Random

genotypeLength = 8
targetPhenotype = 5
mutationsPerGenotype = 7

instance Bounded Gene where
    minBound = toEnum 0
    maxBound = last $ [minBound :: Gene ..]

instance Random Gene where
    randomR (a,b) g = 
        case (randomR (fromEnum a, fromEnum b) g) of
            (x, g') -> (toEnum x, g')
    random g = randomR (minBound, maxBound) g

-- | Create a new genotype consisting of randomly chosen genes.
mkgenotype :: RandomGen g => g -> (Genotype, g)
mkgenotype = f' genotypeLength
    where f' 0 g = ([], g)
          f' n g = let (x, g') = rgene g
                       (xs, g'') = f' (n-1) g'
                   in (x:xs, g'')

-- | Return a random gene.
rgene :: RandomGen g => g -> (Gene, g)
rgene g = randomR (minBound, maxBound) g

-- | Remove element at a specific index position from a list.
lsdrop :: [a] -> Int -> [a]
lsdrop [] _ = []
lsdrop xs i = let (a, b) = splitAt i xs in a ++ (drop 1 b)

-- | Return n random elements from a list.
rpick :: RandomGen g 
      => [a] 
      -> Int 
      -> g 
      -> ([a], g)
rpick _ 0 g = ([], g)
rpick xs n g = let (ri, g') =   randomR range g
                   (xs', g'') = rpick (lsdrop xs ri) (n-1) g'
               in ((xs !! ri):xs', g'')
    where range = (0, (length xs)-1)

-- | Apply a queue of functions to n random elements in a list.
--
-- rmap' has essentially the same signature as rmap except that the second arg
-- is a list of elements with their corresponding ordinal positions instead of
-- just a list of elements ([(a, Int)] instead of [a]).
rmap :: (RandomGen g) 
     => (a -> g -> (a, g))  -- ^ Function to apply to randomly selected element.
     -> [a]                 -- ^ List to partially, randomly map.
     -> Int                 -- ^ Number of random elements to apply function to.
     -> g                   -- ^ Random number generator.
     -> ([a], g)            -- ^ Partially, randomly mapped list and random gen.
rmap _ [] _ g = ([], g)
rmap f xs n g = let (hotxs, g') = rpick [0..(length xs)-1] n g 
                in rmap' f (zip xs [0..]) hotxs g'
    where rmap' :: RandomGen g
                => (a -> g -> (a, g)) 
                -> [(a, Int)] 
                -> [Int]
                -> g 
                -> ([a], g)
          rmap' _ [] _ g = ([], g)
          rmap' f ((x, i):xs) hxs g
              | elem i hxs    = let (x', g') =    f x g
                                    (xs', g'') =  rmap' f xs hxs g'
                                in  (x':xs', g'')
              | otherwise     = let (xs', g') =   rmap' f xs hxs g
                                in  (x:xs', g')

-- | Calculate the deviation between the target phenotype and observed 
-- phenotype. The deviation is a float between zero and infinity: zero meaning
-- the candidate phenotype matched the target phenotype; infinity meaning that
-- the candidate couldn't be evaluated and therefore had no observable 
-- phenotype.
calcDeviation :: (Genotype, Maybe Phenotype)
              -> (Genotype, Maybe Phenotype, Float)
calcDeviation (eg, p) = (eg, p, dev p)
    where dev (Just observedPT) = abs (read observedPT - targetPhenotype)
          dev Nothing = 1/0 -- Infinity

sortByDeviation :: (Ord c) => [(a, b, c)] -> [(a, b, c)]
sortByDeviation = sortBy (\(_, _, a) (_, _, b) -> compare a b)

evolve :: RandomGen g
       => g
       -> [(Genotype, Maybe Phenotype, Float)]
       -> (Population, g)
evolve g [] = ([], g)
evolve g (p:ps) = let (p', g') = mutate (genotype p) g
                      (ps', g'') = evolve g' ps
                  in (p':ps', g'')
    where mutate gt g = rmap (\_ g -> rgene g) gt mutationsPerGenotype g

evalPopulation :: Population -> IO [(Genotype, Maybe Phenotype, Float)]
evalPopulation = fmap (sortByDeviation . map calcDeviation) . 
                 sequence . map eval

seedPopulation :: RandomGen g 
               => Int 
               -> g 
               -> (Population, g)
seedPopulation 0 g = ([], g)
seedPopulation n g = let (p, g') =   mkgenotype g
                         (ps, g'') = seedPopulation (n-1) g'
                     in (p:ps, g'')

solutions :: [(Genotype, Maybe Phenotype, Float)] -> [Genotype]
solutions = map genotype . filter (\(_, _, d) -> d == 0)

genotype :: (Genotype, Maybe Phenotype, Float) -> Genotype
genotype (x, _, _) = x

solve :: RandomGen g 
      => IO Population 
      -> g
      -> IO [Genotype]
solve p g = do
    p <- p
    ep <- evalPopulation p
    let (nxp, g') = evolve g ep
        s = solutions ep
    if length s > 0 then return s
    else solve (return nxp) g'

main = do
    let (p, g') = seedPopulation 10 g
    ss <- solve (return p) g'
    return (head ss)
    where g = mkStdGen 5


