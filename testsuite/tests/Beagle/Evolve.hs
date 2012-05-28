-- | First attempt at haskell unit tests.
import Beagle.Evolve
import System.Random
import Test.HUnit

rand = mkStdGen 1

tests = test [
    'a'  ~=? (fst $ tournamentSelection [('a', 3), ('b', 5)] 2 rand),
    'c'  ~=? (fst $ tournamentSelection [('a', 3), ('b', 10), ('c', 4), ('d', 1)] 2 rand),
    'd'  ~=? (fst $ tournamentSelection [('a', 3), ('b', 10), ('c', 4), ('d', 1)] 3 rand),
    "A2345678" ~=? (fst $ crossover "ABCDEFGH" "12345678" 1 rand),
    "A23D5FGHI0123456" ~=? (fst $ crossover "ABCDEFGHIJKLMNOP" "1234567890123456" 5 rand)
    ]

main = runTestTT tests

