-- | As an alternative to evaluating a genotype in a domain-specific function,
-- perhaps they could be evaluated in Haskell. The functional nature of Haskell
-- lends itself well to expressing a complex program as a sequence of "program
-- blocks" which is in keeping with the genetic algorithm's genotype: a complete
-- organism definition in a sequence of blocks that can be arbitrarily mutated
-- without total destruction of the organism's structure.

import Data.Dynamic
import qualified Data.Map as Map
import Data.Maybe (fromJust)

-- | Available program blocks --------------------------------------------------

digit :: Int -> Maybe Int -> Maybe Int
digit x Nothing = Just x
digit x (Just y) = Just (read (show x ++ show y))

digit1 :: Maybe Int -> Maybe Int
digit1 = digit 1

digit2 :: Maybe Int -> Maybe Int
digit2 = digit 2

digit3 :: Maybe Int -> Maybe Int
digit3 = digit 3

digit4 :: Maybe Int -> Maybe Int
digit4 = digit 4

add' :: Maybe Int -> Maybe Int -> Maybe Int
add' (Just x) (Just y) = Just (x + y)

max' :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
max' (Just a) (Just b) (Just c) = Just (max a (max b c))

stop = Nothing :: Maybe Int

data Gene = Digit1 | Digit2 | Digit3 | Digit4 | Add | Max | Stop 
    deriving (Eq, Ord)

geneMap :: Map.Map Gene Dynamic
geneMap = Map.fromList [
    (Digit1, toDyn digit1),
    (Digit2, toDyn digit2),
    (Digit3, toDyn digit3),
    (Digit4, toDyn digit4),
    (Add,    toDyn add'),
    (Max,    toDyn max'),
    (Stop,   toDyn stop)
    ] 

-- | Evaluation ----------------------------------------------------------------

-- | Evaluate a sequence of progam blocks.
eval :: [Dynamic]       -- ^ Unprocessed input stack.
     -> Maybe [Dynamic] -- ^ Working stack; potentially partially folded
     -> Maybe [Dynamic] -- ^ Final stack; should contain a single final value.
eval _ Nothing     = Nothing
eval [] (Just stk) = Just stk
eval (x:xs) (Just stk) 
    | xtype == typeOf stop = eval xs (foldl foldf (Just [x]) stk)
    | otherwise            = eval xs (Just (x:stk))
    where xtype = dynTypeRep x
          foldf (Just stk') x
              | isfunc (head stk') = Just (stk' ++ [x])
              | otherwise = case dynApply x (head stk') of
                               Just x  -> Just [x]
                               Nothing -> Nothing

-- | Determine whether a dynamically-typed value is a function.
isfunc :: Dynamic -> Bool
isfunc x = typeRepTyCon (dynTypeRep x) == typeRepTyCon (typeOf2 id)

-- | Convert list of genes to list of gene functions.
initgene :: [Gene] -> [Dynamic]
initgene = map (\x -> (Map.!) geneMap x)

-- | Main ----------------------------------------------------------------------

-- | An attempt to express a program (with functions and control structures)
-- as a series of blocks.
sq :: [Gene]
sq = [Max, Add, Digit1, Digit2, Stop, Digit2, Digit3, Stop, Digit3, Digit4,
      Stop, Digit1, Stop]

main = return . display $ eval (initgene sq) (Just [])

display :: Maybe [Dynamic] -> Maybe Int
display (Just x) = fromDyn (head x) Nothing :: Maybe Int 
display Nothing = Nothing

