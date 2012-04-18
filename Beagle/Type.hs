module Beagle.Type
    ( Gene (..)
    , Genotype
    , Phenotype
    , Population
    ) where

-- | Building blocks of a genotype.
data Gene = Digit0
          | Digit1
          | Digit2
          | Digit3
          | Digit4
          | Digit5
          | Digit6
          | Digit7
          | Digit8
          | Digit9
          | OpPlus
          | OpMinus
          | OpMult
          | OpDiv
          | Empty
          deriving (Ord, Eq, Show, Enum)

-- | Definition of a candidate solution.
type Genotype = [Gene]

-- | The composite of a candidate solution's observable characteristics.
type Phenotype = String

-- | Group of genotypes that are evaluated together; a generation.
type Population = [Genotype]

