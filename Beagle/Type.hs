module Beagle.Type
    ( Genotype
    , Population
    , Score
    ) where

import Beagle.Domain

-- | Definition of a candidate solution.
type Genotype = [Gene]

-- | Group of genotypes that are evaluated together; a generation.
type Population = [Genotype]

-- Float between 0 and 1, indicating how close a genotype's observed phenotype
-- was to the target phenotype.
type Score = Float

