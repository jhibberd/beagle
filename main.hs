-- | Genetic Algorithm

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

-- | The composite of a candidate solution's observable characteristics.
type Phenotype = String

-- | Representation of a genotype that can be evaluated by the fitness function.
type EncodedGenotype = String

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
rpick :: (RandomGen g) => [a] -> Int -> g -> [a]
rpick _ 0 _ = []
rpick xs n g = let (ri, g') = randomR range g 
               in (xs !! ri):(rpick (lsdrop xs ri) (n-1) g')
    where range = (0, (length xs)-1)

-- | Apply a queue of functions to n random elements in a list.
rmap :: (RandomGen g) 
     => [(a -> a)]      -- ^ List of map functions to apply to random elements. 
                        --   The list of treated as a queue, popping a function, 
                        --   applying it, then discarding it.
     -> [a]             -- ^ List to partially, randomly map.
     -> Int             -- ^ Number of random elements to apply function to.
     -> g               -- ^ Random number generator.
     -> [a]             -- ^ Partially, randomly mapped list.
rmap _ [] _ _ = []
rmap fs xs n g = rmap' fs (zip xs [0..])
    where hotxs = rpick [0..(length xs)-1] n g
          rmap' :: [(a -> a)] -> [(a, Int)] -> [a]
          rmap' _ [] = []
          rmap' (f:fs) ((x, i):xs)
              | elem i hotxs  = f x:rmap' fs xs
              | otherwise     = x:rmap' (f:fs) xs

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
encodeGenotype :: Genotype -> EncodedGenotype
encodeGenotype = map (\x -> blockstrMap Map.! x) . filter (/=Empty)

-- | Evaluate definition using the domain server.
evalFitness :: EncodedGenotype -> IO (EncodedGenotype, Phenotype)
evalFitness domdef = fmap (\x -> (domdef, x)) result
    where host = "http://localhost:1831/"
          result = simpleHTTP (getRequest (host++domdef)) >>= getResponseBody

-- MAIN ------------------------------------------------------------------------

main = sequence . map (evalFitness . encodeGenotype) $ population
    where makeGenotype g = rmap (functionize $ rblockfeed g) (newdef 10) 7 g
          makeRandomGenotype (f, i) = f $ mkStdGen i
          population = map makeRandomGenotype $ zip (repeat makeGenotype) [0..9]

   
