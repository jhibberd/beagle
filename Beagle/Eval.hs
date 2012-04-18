-- | Evaluate the fitness of a candidate solution using the domain evaluation
-- server.

module Beagle.Eval
    ( eval 
    ) where

import Beagle.Type
import qualified Data.Map as Map
import Network.HTTP

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
encodeGenotype :: Genotype -> String
encodeGenotype = map (\x -> blockstrMap Map.! x) . filter (/=Empty)

-- | Evaluate definition using the domain server.
eval :: Genotype -> IO (Genotype, Maybe Phenotype)
eval domdef = fmap (\x -> (domdef, x)) $ fmap parse result
    where host = "http://localhost:1831/"
          result = simpleHTTP (getRequest (host++encodedGT)) >>= getResponseBody
          parse ('0':xs) = Just xs
          parse ('1':xs) = Nothing
          encodedGT = encodeGenotype domdef
