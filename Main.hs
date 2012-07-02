import Beagle
import System.Random

main = do 
    let (p, g') = mkPopulation (mkStdGen 0)
    g <- search p g' 1
    print g
