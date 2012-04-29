module Beagle.Eval2
    ( eval
    ) where

import Beagle.Domain as D
import Beagle.Type
import qualified Data.Map as Map
import Debug.Trace

eval :: Genotype -> Int -> State -> (Genotype, Float)
eval g i s 
    | i == length g = traceShow final final
    | otherwise = let (g', i', s') = f g i s
                  in eval g' i' s'
        where f = (Map.!) D.gmap (g !! i)
              final = (g, D.score s)
