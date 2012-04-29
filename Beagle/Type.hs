module Beagle.Type
    ( Genotype
    , Population
    , Score
    , Arity
    ) where

import Beagle.Domain

-- | Definition of a candidate solution.
type Genotype = [Gene]

-- | Group of genotypes that are evaluated together; a generation.
type Population = [Genotype]

-- Float between 0 and 1, indicating how close a genotype's observed phenotype
-- was to the target phenotype.
type Score = Float

-- | The number of arguments that need to be applied to a gene. All genes are
-- interpreted as functions, however a function with no arguments is 
-- essentially a constant value.
type Arity = Int

