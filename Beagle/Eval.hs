-- | Evaluate the fitness of a candidate solution using the domain evaluation
-- server.

-- | Explain about usage of unsafePerformIO

module Beagle.Eval
    ( eval
    ) where

import Beagle.Type
import qualified Data.Map as Map
import Network.HTTP
import System.IO.Unsafe (unsafePerformIO)

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

