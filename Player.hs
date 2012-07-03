import Beagle
import Control.Monad
import System.IO

-- | TODO:
genome :: Scenario -> Scenario
genome = solve [4,4,0,2,5,5,5,7,5,7,3,3,4,1,0,1,5,4,5,5,0,6,0,4,1,3,8,5,8,1,4,3,0,1,3,6,3,6,6,3,4,2,3,6,1,7,5,6,4,2,3,5,6,0,0,1,5,2,3,5,8,7,8,4,7,4,7,3,4,8,5,2,6,2,7,3,5,2,7,2,4,1,1,1,2,0,3,0,7,4,1,7,0,1,0,3,6,5,1,6,3,0,3,7,6,8,5,2,5,5,5,8,7,1,7,1,4,7,5,8,5,4,3,3,7,6,8,5,0,7,5,8,2,5,5,0,6,1,1,5,7,5,8,1,1,8,6,4,8,0,0,6,4,3,3,6,2,1,7,5,2,1,7,7,3,1,6,0,7,0,3,3,4,7,1,2,6,8,7,0,3,4,7,5,2,4,2,6,8,7,0,6,6,2,3,7,3,3,7,5,0,5,7,3,7,0,8,7,2,7,6,6,7,3,4,8,5,6,1,6,8,0,7,5,3,2,5,4,7,4,8,3,2,1,5,3,0,7,4,8,1,7,4,2,8,3,5,6,6,7,3,7,0,2,6,1,7,3,6,0,6,5,3,0,0,7,5,2,8,4,6,2,7,4,0,1,5,3,8,4,2,3,7,0,6,5,1,0,7,5,6,8,7,2,6,8,6,4,3,7,4,5,3,3,7,7,5,6,2,8,1,7,4,0,2,6,0,2,0,3,4,7,7,6,6,4,5,3,6,8,4,6,4,0,1,2,4,4,8,1,4,8,7,4,0,3,1,5,5,8,2,8,7,1,2,1,7,8,5,3,0,4,8,4,8,7,2,0,0,5,6,8,1,7,2,5,1,5,5,7,0,4,8,1,6,1,1,2,8,3,4,6,4,4,3,0,0,7,3,3,6,2,3,4,5,7,6,8,1,4,0,8,5,0,5,5,2,8,5,8,7,4,0,6,4,7,4,8,0,5,6,2,3,0,1,5,6,4,8,5,7,4,5,0,5,4,4,5,8,6,3,0,2,0,1,3,8,6,8,8,0,1,7,1,3,8,0,2,1,1,8,1,1,0,7,4,4,0,6,3,7,7,6,5,0,0,1,7,1,3,7,6,0,5,2,8,2,7,8,6,0,1,7,3,7,6,6,2,5,7,6,8,4,6,8,1,8,5,7,5,5,4,2,1,6,8,6,8,2,4,6,4,7,2,1,0,6,2,8,0,4,6,4,8,6,2,1,4,7,2,6,8,1,0,6,5,1,0,5,3,3,4,6,1,5,6,7,6,8,7,5,6,0,0,0,8,7,8,7,4,2,0,4,1,1,0,2,0,8,5,1,3,4,5,7,6,3,8,0,3,2,4,5,4,2,1,0,5,3,6,4,7,4,5,5,7,7,0,5,1,2,5,2,1,8,2,3] 

assignState :: Scenario -> Int
assignState scn
    | isWinner scn =                    0
    | isWinner $ invertPlayers scn =    1
    | isDraw scn =                      2
    | otherwise =                       3

main = forever $ do
    scnStr <- getLine
    let scn = fromKey $ read scnStr
        state = assignState scn
        (scn'', state'') = if state == 3
            then let scn' = genome scn
                     state' = assignState scn'
                 in (scn', state')
            else (scn, state)
    putStrLn $ (show state'' ++ (show $ toKey scn''))
    hFlush stdout -- TODO: What if we remove this?