-- | Evaluate the fitness of a candidate solution using the domain evaluation
-- server.

module Beagle.Eval
    ( eval
    ) where

import Beagle.Domain
import Beagle.Type
import Data.Dynamic
import Debug.Trace
import qualified Data.Map as Map

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

eval :: Genotype -> (Genotype, Maybe Phenotype)
eval gt = trace (show x) x
    where x = (gt, display $ eval' (initgene gt) stk)
          stk = Just []

display :: Maybe [Dynamic] -> Maybe String
display (Just x) = fmap show $ (fromDyn (head x) Nothing :: Maybe Int)
display Nothing = Nothing

