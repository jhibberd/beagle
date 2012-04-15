-- | Genetic Algorithm

import Data.List (sortBy)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Network.HTTP
import System.Random

-- | Building blocks for a definition.
data Block = Digit0
           | Digit1
           | Digit2
           | Digit3
           | Digit4
           | Digit5
           | Digit6
           | Digit7
           | Digit8
           | Digit9
           | OpPlus
           | OpMinus
           | OpMult
           | OpDiv
           | Empty
           deriving (Ord, Eq, Show, Enum)

-- | A definition is a series of blocks that is evaluated.
-- TODO(jhibberd) Block to Gene
type Genotype = [Block]

type Population = [Genotype]

-- | The composite of a candidate solution's observable characteristics.
type Phenotype = String

-- | Build a new definition containing n Empty blocks.
newdef :: Int -> Genotype
newdef n = take n $ repeat Empty

instance Bounded Block where
    minBound = toEnum 0
    maxBound = last $ [minBound :: Block ..]

instance Random Block where
    randomR (a,b) g = 
        case (randomR (fromEnum a, fromEnum b) g) of
            (x, g') -> (toEnum x, g')
    random g = randomR (minBound, maxBound) g

-- | Return an infinite list of random blocks.
rblockfeed :: (RandomGen g) => g -> [Block]
rblockfeed g = randomRs (minBound, maxBound) g

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

main = return $ rmap f [1..10] 5 (mkStdGen 1)
    where f :: RandomGen g => Int -> g -> (Int, g)
          f x g = randomR (1, 1000) g

-- | Convert a list of values to a list of functions that return the value. 
functionize :: [a] -> [(a -> a)]
functionize xs = map (\x -> (\_ -> x)) xs

-- DOMAIN LANGUAGE -------------------------------------------------------------

-- | Map block tokens to their string counterparts in the domain language.
blockstr = [
    (Digit0, '0'),
    (Digit1, '1'),
    (Digit2, '2'),
    (Digit3, '3'),
    (Digit4, '4'),
    (Digit5, '5'),
    (Digit6, '6'),
    (Digit7, '7'),
    (Digit8, '8'),
    (Digit9, '9'),
    (OpPlus, '+'),
    (OpMinus, '-'),
    (OpMult, '*'),
    (OpDiv, '/')
    ]
blockstrMap :: Map.Map Block Char
blockstrMap = Map.fromList blockstr

-- | Map all non-Empty blocks of a definition to their domain language 
-- counterpart
encodeGenotype :: Genotype -> String
encodeGenotype = map (\x -> blockstrMap Map.! x) . filter (/=Empty)

-- | Evaluate definition using the domain server.
evalFitness :: Genotype -> IO (Genotype, Maybe Phenotype)
evalFitness domdef = fmap (\x -> (domdef, x)) $ fmap parse result
    where host = "http://localhost:1831/"
          result = simpleHTTP (getRequest (host++encodedGT)) >>= getResponseBody
          parse ('0':xs) = Just xs
          parse ('1':xs) = Nothing
          encodedGT = encodeGenotype domdef

-- | Calculate the deviation between the target phenotype and observed 
-- phenotype. The deviation is a float between zero and infinity: zero meaning
-- the candidate phenotype matched the target phenotype; infinity meaning that
-- the candidate couldn't be evaluated and therefore had no observable 
-- phenotype.
calcDeviation :: (Genotype, Maybe Phenotype)
              -> (Genotype, Maybe Phenotype, Float)
calcDeviation (eg, p) = (eg, p, dev p)
    where targetPT = 5
          dev (Just observedPT) = abs (read observedPT - targetPT)
          dev Nothing = 1/0 -- Infinity

sortByDeviation :: (Ord c) => [(a, b, c)] -> [(a, b, c)]
sortByDeviation = sortBy (\(_, _, a) (_, _, b) -> compare a b)

-- MAIN ------------------------------------------------------------------------

-- take 1 from this
{-
solve :: Generation -> Genotype
solve g = evaluateGeneration . pickMostPromisingCandidates . 
          extractAnyPerfectCandidates . usePromisingCandidateToBuildNewGen
          returnPerfectCandidate ++ solve newGen
solve p = evaluateGeneration p
-}

-- TODO(jhibberd) Re-engineer all functions that accept RandomGen to also return
-- the "incremented" RandomGen

{-
evolve :: RandomGen g
       => IO [(Genotype, Maybe Phenotype, Float)]
       -> g 
       -> IO Population
evolve p g = fmap (map (mutate . genotype)) p
    where genotype (a, _, _) = a
          mutate gt = rmap (functionize $ rblockfeed g) gt 7 g
-}

evalPopulation :: Population -> IO [(Genotype, Maybe Phenotype, Float)]
evalPopulation = fmap (sortByDeviation . map calcDeviation) . 
                 sequence . map evalFitness
{-
seedPopulation :: RandomGen g 
               => Int 
               -> g 
               -> (Population, g)
seedPopulation 0 g = ([], g)
seedPopulation n g = let (p, g') =   grow g
                         (ps, g'') = seedPopulation (n-1) g'
                     in (p:ps, g'')
    where grow g''' = rmap (functionize $ rblockfeed g''') (newdef 10) 7 g'''
-}
{-
main = let (p, g') = seedPopulation 10 g 
       in evalPopulation p
    where g = mkStdGen 5
-}

