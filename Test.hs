
-- | First attempt at haskell unit tests.
import Beagle
import System.Random
import Test.HUnit
import qualified Data.Set as Set
import System.IO.Unsafe

rand = mkStdGen 1

mockGenome :: Scenario -> Scenario
mockGenome scn = play scn 0

tests = test [
    --'a'  ~=? (fst $ tournamentSelection [('a', 3), ('b', 5)] 2 rand),
    --'c'  ~=? (fst $ tournamentSelection [('a', 3), ('b', 10), ('c', 4), ('d', 1)] 2 rand),
    --'d'  ~=? (fst $ tournamentSelection [('a', 3), ('b', 10), ('c', 4), ('d', 1)] 3 rand),

    --0.0 ~=? score [True, True, True, True],
    --1.0 ~=? score [False, False, False, False],
    --0.5 ~=? score [True, True, False, False],

    --[False] ~=? (unsafePerformIO $ playGame mockGenome 1 [0, 1, (-1), (-1), (-1), 1, (-1), 1, 1]),
    --[False, True] ~=? (unsafePerformIO $ playGame mockGenome 1 [0, 1, 0, 1, (-1), 1, 1, (-1), (-1)]),
    --[True] ~=? (unsafePerformIO $ playGame mockGenome 1 [0, 0, 0, 1, (-1), 1, 1, (-1), (-1)]),
    --[False, False] ~=? (unsafePerformIO $ playGame mockGenome 1 [0, 0, 0, 1, (-1), 1, (-1), 1, (-1)]),
    [False, False] ~=? (unsafePerformIO $ playGame mockGenome 1 [0, 0, 0, 1, 0, 0, (-1), 1, (-1)]),

    [1, 0, 0, 0, 0, 0, 0, 0, 0] ~=? play [0, 0, 0, 0, 0, 0, 0, 0, 0] 0,
    [1, (-1), 0, 0, 0, 0, 0, 0, 0] ~=? play [1, 0, 0, 0, 0, 0, 0, 0, 0] 0,
    [1, (-1), 1, (-1), (-1), 0, 1, (-1), 1] ~=? play [1, (-1), 1, (-1), 0, 0, 1, (-1), 1] 0,
    [1, (-1), 1, (-1), 0, (-1), 1, (-1), 1] ~=? play [1, (-1), 1, (-1), 0, 0, 1, (-1), 1] 1,
    [1, (-1), 1, (-1), (-1), 0, 1, (-1), 1] ~=? play [1, (-1), 1, (-1), 0, 0, 1, (-1), 1] 2


    --"ABCDEFGhijkLMNOpqRStuvwxYZ" ~=? (fst $ crossover' (zip ['a'..'z'] ['A'..'Z']) rand False 0.2),
    --"ABCDEFGHIJKLMNOPQRSTUVWXYZ" ~=? (fst $ crossover' (zip ['a'..'z'] ['A'..'Z']) rand False 0.0),
    --"abcdefghij0lmnopqrstuvwxyz" ~=? (fst $ mutate' ['a'..'z'] rand 0.05 (\g -> ('0', g)))
    --"A23D5FGHI0123456" ~=? (fst $ crossover "ABCDEFGHIJKLMNOP" "1234567890123456" 5 rand),
    --Set.empty ~=? sublists "hello",
    --5 ~=? diversity ["hello", "ballo"]
    ]


main = runTestTT tests

