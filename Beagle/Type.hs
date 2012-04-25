module Beagle.Type
    ( Genotype
    , Phenotype
    , Population
    , Delta
    , genotype
    , delta
    ) where

import Beagle.Domain

-- | Definition of a candidate solution.
type Genotype = [Gene]

-- | The composite of a candidate solution's observable characteristics.
type Phenotype = String

-- | Group of genotypes that are evaluated together; a generation.
type Population = [Genotype]

-- | Single numeric value that expresses the deviation between an observed
-- phenotype and the target phenotype.
type Delta = Float

-- | Functions for extracting values from evaluated genotypes rather than using
-- ordinal positions. Functions are preferred over haskell record syntax.

genotype :: (Genotype, Maybe Phenotype, Delta) -> Genotype
genotype (x, _, _) = x

delta :: (Genotype, Maybe Phenotype, Delta) -> Delta
delta (_, _, x) = x
