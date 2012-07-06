import Beagle
import System.Random

main = do 
    setStdGen (mkStdGen 0)
    p <- mkPopulation
    g <- search p 1
    print g
