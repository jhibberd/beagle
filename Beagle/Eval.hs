-- | Evaluate the fitness of a candidate solution using the domain evaluation
-- server.

-- | Explain about usage of unsafePerformIO

module Beagle.Eval
    ( eval2
    ) where

import Debug.Trace

import Beagle.Domain
import Beagle.Type
import Data.Dynamic
import qualified Data.Map as Map
import Network.HTTP
import System.IO.Unsafe (unsafePerformIO)
{-
httpEndpoint = "http://localhost:1831/"

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
blockstrMap :: Map.Map Gene Char
blockstrMap = Map.fromList blockstr

-- | Map all non-Empty blocks of a definition to their domain language 
-- counterpart
encode :: Genotype -> String
encode = map (\x -> blockstrMap Map.! x) . filter (/=Empty)

-- | Evaluate definition using the domain server.
eval :: Genotype -> (Genotype, Maybe Phenotype)
eval genotype = let phenotype = parse get in (genotype, phenotype)
    where get = unsafePerformIO (simpleHTTP (getRequest url) 
                                 >>= getResponseBody)
          parse ('0':xs) = Just xs
          parse ('1':xs) = Nothing
          url = httpEndpoint ++ encode genotype
-}

-- | Evaluation using only haskell ---------------------------------------------

-- | Evaluate a sequence of progam blocks.
eval' :: [Dynamic]       -- ^ Unprocessed input stack.
     -> Maybe [Dynamic] -- ^ Working stack; potentially partially folded
     -> Maybe [Dynamic] -- ^ Final stack; should contain a single final value.
eval' _ Nothing     = Nothing
eval' [] (Just stk) = Just stk
eval' (x:xs) (Just stk) 
    | xtype == typeOf stop = eval' xs (foldl foldf (Just [x]) stk)
    | otherwise            = eval' xs (Just (x:stk))
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
initgene = map (\x -> (Map.!) genemap x) . filter (/=Empty)

eval2 :: Genotype -> (Genotype, Maybe Phenotype)
eval2 gt = trace (show x) x
    where x = (gt, display $ eval' (initgene gt) (Just []))

display :: Maybe [Dynamic] -> Maybe String
display (Just x) = fmap show $ (fromDyn (head x) Nothing :: Maybe Int)
display Nothing = Nothing

