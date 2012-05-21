-- | To improve insight into how the genetic algorithm is evolving the 
-- genotypes key events are written to log files.
--
-- Each file holds data on a single event type for a single generation.

module Beagle.Log
    ( generation
    , eval
    , setUp
    , score
    , evolve
    ) where

import Control.Monad
import Data.HashTable
import System.Directory

-- | Algorithm has been started.
setUp :: IO ()
setUp = do
    removeDirectoryRecursive logDir
    createDirectory logDir

-- | Delete logs files associated with old generations to ensure disk usage
-- doesn't exceed a predefined threshold.
maybeCropLogs :: IO ()
maybeCropLogs = do
        g <- fmap read $ readFile generationFile
        let t = g - threshold
        when (t > 0) $ deleteLogs t
    where deleteLogs t = do
              removeFile $ logPath' t "genotype" 
              removeFile $ logPath' t "eval" 
              removeFile $ logPath' t "score" 
              removeFile $ logPath' t "evolve" 
          threshold = 20

-- | A new generation has begun.
-- Increment the generation count and log all the genotypes (against their
-- hashes).
generation :: (Show a) => Int -> [a] -> IO ()
generation !gen ps = do
        writeFile generationFile (show gen)
        maybeCropLogs
        p <- logPath "genotype"
        sequence $ map (writeGenotype p) ps
        return ()
    where writeGenotype p gt = write p (hash gt ++ "," ++ show gt)

-- | A genotype has been evaluated.
eval :: (Show a, Show b) => [a] -> b -> IO ()
eval !gt !s = do
    p <- logPath "eval"
    write p (hash gt ++ "," ++ show s)

-- | A genotype has been scored.
score :: (Show a) => [a] -> Float -> IO ()
score !gt !s = do
    p <- logPath "score"
    write p (hash gt ++ "," ++ show s)

-- | Using crossover and mutation two genotypes have been combined to create
-- a third, new generation, genotype.
evolve :: (Show a) => [a] -> [a] -> [a] -> IO ()
evolve !a !b !c = do 
    p <- logPath "evolve"
    write p (hash a ++ "," ++ hash b ++ "," ++ hash c)

-- | Helpers and constants -----------------------------------------------------

logDir = "/tmp/beagle"
generationFile = logDir ++  "/generation"

write :: String -> String -> IO ()
write path txt = appendFile path (txt ++ "\n")

logPath :: String -> IO String
logPath fileType = do
    g <- readFile generationFile
    return (logPath' (read g) fileType)

logPath' :: Int -> String -> String
logPath' gen fileType = logDir ++ "/" ++ show gen ++ "." ++ fileType

hash :: (Show a) => a -> String
hash = show . hashString . show

