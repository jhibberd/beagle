import System.Random

import Control.Monad.State

--import Data.Random (runRVar)
--import Data.Random.Source.DevRandom
--import Data.Random.Extras (choice)

data Block = Digit Int      -- ^ 0..9
           | Operator Char  -- ^ +, -, /, or *
           | Empty          -- ^ None
           deriving (Show)

type DNA = [Block]

-- | Build a random DNA sequence containing n Empty blocks.
buildDNA :: Int -> DNA
buildDNA n = take n $ repeat Empty

-- | Mutate n random blocks in a DNA sequence.
--mutateDNA :: DNA -> Int -> IO [Block]
--mutateDNA dna n = sequence [randBlock | _ <- [1..10]]
--mutateDNA dna n = fmap maybeMutate . zip dna [0..]
--    where maybeMutate block i = 

-- | Apply a function to n random elements in a list.
--rmap :: (RandomGen g) => IncrF a -> Int -> [a] -> g -> [a]
--rmap f n xs g = map (\(x, i) -> if i `elem` hotxs then let (x, f') = f x in x else x) $ zip xs [0..]
--    where hotxs = rpick [0..(length xs)-1] n g

-- | Return n random elements from a list.
rpick :: (RandomGen g) => [a] -> Int -> g -> [a]
rpick _ 0 _ = []
rpick xs n g = let (ri, g') = randomR range g 
               in (xs !! ri):(rpick (lsdrop xs ri) (n-1) g')
    where range = (0, (length xs)-1)

-- | Remove element at a specific index position from a list.
lsdrop :: [a] -> Int -> [a]
lsdrop [] _ = []
lsdrop xs i = let (a, b) = splitAt i xs in a ++ (drop 1 b)

-- | Return a random DNA sequence block (and incremented random generator).
randblock :: (RandomGen g) => g -> (Block, g)
randblock g = let (i, g') = randomR range g in (blocks !! i, g')
    where range = (0, (length blocks)-1)
          blocks = [ Digit 0
                   , Digit 1
                   , Digit 2
                   , Digit 3
                   , Digit 4
                   , Digit 5
                   , Digit 6
                   , Digit 7
                   , Digit 8
                   , Digit 9
                   , Operator '+'
                   , Operator '-'
                   , Operator '*'
                   , Operator '/'
                   ]

-- | 1.

type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

stackm :: State Stack Int
stackm = do
    push 3
    push 2
    push 1
    pop

--main = return (runState (push 3 >>= (\_ -> push 2)) [])

-- | 2.

rpop :: (RandomGen g) => State g Int
rpop = state $ \g -> let (n, g') = randomR (0,100) g in (n, g')

poplots :: (RandomGen g) => State g Int
poplots = do
    a <- rpop
    b <- rpop
    rpop

main = return (runState poplots (mkStdGen 97))

--main = mutateDNA (buildDNA 10) 2
--main = return . take 10 $ randBlockFeed
--main = return $ rmap id 3 [1..10]
--main = return $ setmove (Set.fromList [1..5]) (Set.fromList []) 3
--main = return $ rpick [1..10] 5 (mkStdGen 104)
--main = return $ rmap (\x -> 'x') 3 ['a'..'g'] (mkStdGen 98)
--main = return $ let (i, g) = randblock (mkStdGen 97) in i
--main = return $ rmap (f g) 5 (buildDNA 10) g
--    where g = mkStdGen 40
--          f g x = let (i, g') = randblock g in (i, f g')

