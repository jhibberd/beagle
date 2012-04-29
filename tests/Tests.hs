-- | First attempt at haskell unit tests.
import Beagle.Domain
import Test.HUnit

tests = test [
    3 ~=? score [Incr, Incr, Incr, Add],
    0 ~=? score [Add],
    0 ~=? score [Incr],
    2 ~=? score [Incr, Incr, Incr, Add, Incr, Subtract],
    0 ~=? score [Divide]
    ]

main = runTestTT tests

