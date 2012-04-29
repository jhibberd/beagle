-- | First attempt at haskell unit tests.
import Beagle.Domain
import Beagle.Eval
import Test.HUnit

-- | Syntactic shortcut for evaluating genotype.
f = snd . eval

tests = test [
    "Single gene" ~: 1 ~=? f [Digit1, Runner''],
    "Basic function" ~: 4 ~=? f [Digit1, Digit3, Plus, Runner''],
    "Bad basic function" ~: 1 ~=? f [Digit3, Plus, Runner''],
    "2 functions" ~: 15 ~=? f [Digit2, Digit1, Stick, Digit3, Plus, Runner''],
    "Basic if" ~: 3 ~=? f [Digit1, Digit2, Digit3, Switch, Runner''],
    "If with function" ~: 4 ~=? 
        f [Digit2, Digit3, Minus, Digit3, Digit2, 
           Digit2, Plus, Switch, Runner''],
    "If with runner" ~: 1 ~=?
        f [Digit2, Digit1, Switch, Runner],
    "Basic runner'" ~: 0 ~=? f [Digit2, Digit3, Plus, Runner'],
    "Basic runner', slightly off" ~: 3 ~=? f [Digit2, Runner']
    ]

main = runTestTT tests

