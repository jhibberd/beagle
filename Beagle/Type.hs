module Beagle.Type
    ( Genotype
    , Phenotype
    , Population
    ) where

import Beagle.Domain

-- | Definition of a candidate solution.
type Genotype = [Gene]

-- | The composite of a candidate solution's observable characteristics.
type Phenotype = String

-- | Group of genotypes that are evaluated together; a generation.
type Population = [Genotype]

