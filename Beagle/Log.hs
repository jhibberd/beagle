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

import Data.HashTable
import System.Directory
import System.IO.Unsafe

-- | File to keep track of the current generation
generationFile = "/tmp/beagle/generation"

-- | Update the log file.
write :: String -> String -> b -> b
write path txt dum = const dum $! unsafePerformIO $ 
    appendFile path (txt ++ "\n\n")

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

logPath :: String -> IO String
logPath fileType = do
    g <- readFile "/tmp/beagle/generation"
    return ("/tmp/beagle/" ++ g ++ "." ++ fileType)

-- | A genotype has been evaluated.
eval :: (Show a, Show b) => [a] -> b -> c -> c
eval !gs !s = write (unsafePerformIO $ logPath "eval") ((show . hashString $ show gs) ++ "," ++ show s)

-- | A genotype has been scored.
score :: Float -> b -> b
score !s = write (unsafePerformIO $ logPath "score") ("Score " ++ show s)

-- | Two genotypes have been bred together.
breed :: (Show a) => [a] -> [a] -> [a] -> b -> b
breed !a !b !c = 
    write (unsafePerformIO $ logPath "breed") ("Breed " ++ show a ++ " + " ++ show b ++ " = " ++ show c)

