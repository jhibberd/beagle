import System.Random

data Block = X | A | B | C deriving (Show)
type DNA = [Block]

instance Bounded Block where
  minBound = A
  maxBound = C

instance Random Block where
  randomR (a,b) g = 
      case (randomR (block2Int a, block2Int b) g) of
        (x, g') -> (int2Block x, g')
       where
         block2Int :: Block -> Int
         block2Int A = 0
         block2Int B = 1
         block2Int C = 2

         int2Block :: Int -> Block
         int2Block 0 = A
         int2Block 1 = B
         int2Block 2 = C
  random g = randomR (minBound, maxBound) g

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

-- | TODO(jhibberd)
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

plus10 :: Int -> Int
plus10 a = a+10

plus20 :: Int -> Int
plus20 a = a+20

functionize :: [a] -> [(a -> a)]
functionize xs = map (\x -> (\_ -> x)) xs

--main = return $ rmap (cycle [plus10, plus20]) [1..10] 3 (mkStdGen 99)
--main = return $ rmap (functionize (cycle [99, 88])) [1..10] 3 (mkStdGen 99)
main = return $ rmap (functionize $ rblockfeed g) (replicate 10 X) 7 g
    where g = mkStdGen 99

--main = return . take 6 $ rblockfeed g
--    where g = mkStdGen 101
   
