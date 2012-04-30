-- | Functions that write debug information to log files.

module Beagle.Log
    ( generation
    , eval
    , setUp
    , score
    , breed
    ) where

-- | TODO(jhibberd) Just use builting int hashing function for now.
-- | TODO(jhibberd) Different files for different event types.
-- | TODO(jhibberd) Try and introduce proper IO whilst keeping the program
-- running in constant space.

import System.Directory
import System.IO.Unsafe

-- | File to keep track of the current generation
generationFile = "/tmp/beagle/generation"

-- | Update the log file.
write :: String -> b -> b
write txt dum = const dum $! unsafePerformIO $ 
    appendFile logFile (txt ++ "\n\n")
    where logFile = unsafePerformIO currentLog

setUp :: IO ()
setUp = do
    removeDirectoryRecursive "/tmp/beagle"
    createDirectory "/tmp/beagle"

updateGeneration :: Int -> IO ()
updateGeneration i = writeFile "/tmp/beagle/generation" (show i)

mockIO :: IO a -> b -> b
mockIO io dum = const dum $! unsafePerformIO $ io

-- | A new generation has begun.
generation :: Int -> a -> a
generation !gen = mockIO $ updateGeneration gen

currentLog :: IO String
currentLog = do
    g <- readFile "/tmp/beagle/generation"
    return ("/tmp/beagle/" ++ g)

-- | A genotype has been evaluated.
eval :: (Show a, Show b) => [a] -> b -> c -> c
eval !gs !s = write ("Eval " ++ show gs ++ " -> " ++ show s)

-- | A genotype has been scored.
score :: Float -> b -> b
score !s = write ("Score " ++ show s)

-- | Two genotypes have been bred together.
breed :: (Show a) => [a] -> [a] -> [a] -> b -> b
breed !a !b !c = 
    write ("Breed " ++ show a ++ " + " ++ show b ++ " = " ++ show c)
