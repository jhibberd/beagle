module Beagle.Eval
    ( eval
    ) where

import qualified Beagle.Log as Log
import qualified Data.Map as Map

-- | Runs an initial state through a gene sequence and returns the final state.
--
-- The state (s) and gene (g) data types are defined in the domain module.
eval :: (Ord g, Show s, Show g)
     => [g] -- Gene sequence 
     -- Maps a gene to its function implementation
     -> Map.Map g ([g] -> Int -> s -> ([g], Int, s))
     -- Index (within the gene sequence) of the current gene being applied to
     -- the state.
     -> Int 
     -> s -- Initial (or current, during recursive calls) state
     -> IO s -- Final state 
eval gs gmap i s 
    | i >= length gs = do { Log.eval gs s; return s }
    | otherwise = let (gs', i', s') = f gs i s
                  in eval gs' gmap i' s'
        where f = (Map.!) gmap (gs !! i)
