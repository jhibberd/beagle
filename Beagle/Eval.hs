-- | Evaluate a genotype to produce the observed phenotype.

module Beagle.Eval
    ( eval
    ) where

import Beagle.Domain as D
import Beagle.Type
import Data.Dynamic
import qualified Data.Map as Map

-- | Iterate over the genotype, adding genes to a processing stack. When a gene
-- with an arity > 0 is encountered apply it to neighbouring elements in the
-- stack; thereby reducing the stack to final single value.
--
-- If a genotype is badly formed, and consequently a function application fails,
-- then Nothing is returned.
eval' :: [(Dynamic, Arity)]
      -> Maybe [(Dynamic, Arity)]
      -> Maybe [(Dynamic, Arity)]
eval' _ Nothing = Nothing 
eval' [] stk = stk
eval' (x:xs) (Just stk) = 
    let stk' = Just (x:stk)
    in case x of
        (_, 0) -> eval' xs stk'
        _ ->      eval' xs (stkReduce stk')

-- | Reduce a stack by applying elements with arity > 0 to their neighbouring
-- elements.
--
-- NOTE: The 'null xs' case below allows the genotype to produce a function that
-- hasn't been applied to one of its arguments; the domain-specific runner is
-- resposible for supplying the missing argument and allows the genotype to be
-- tested in multiple scenarios rather than always producing the same value.
stkReduce :: Maybe [(Dynamic, Arity)] -> Maybe [(Dynamic, Arity)]
stkReduce (Just []) = Just []
stkReduce (Just ((x, arity):xs))
    | arity == 0 || null xs  = Just ((x, arity):xs)
    | otherwise = let v = fst $ head xs
                  in case dynApply x v of
                        Just x' ->  stkReduce (Just ((x', arity-1): tail xs))
                        Nothing -> Nothing

-- | Convert list of genes to list of gene functions.
initgene :: [Gene] -> [(Dynamic, Arity)]
initgene = (++[D.runner]) . map (\x -> (Map.!) genemap x) . filter (/=Empty)

-- Prepare a genotype for evaluation by converting gene tokens with their 
-- actual functions, and passing in an empty processing stack.
eval :: Genotype -> (Genotype, Score)
eval gt = (gt, final $ eval' (initgene gt) (Just []))

-- | Attempts to extract and return the final float value from the genotype
-- processing stack.
--
-- Float is between 0 and 1: 0 indicates a perfect match with target phenotype;
-- 1 indicates that the genotype was badly formed and couldn't be evaluated. 
-- Between those two extremes, the closer a value is to 0 the closer the 
-- observed phenotype is to the target phenotype.
final :: Maybe [(Dynamic, Arity)] -> Score
final Nothing   = 1 -- Genotype was badly formed.
final (Just x)  = fromDyn (fst $ head x) 1

