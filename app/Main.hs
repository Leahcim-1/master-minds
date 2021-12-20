module Main where

import Data.Function (on)
import Data.List (isInfixOf, minimumBy)
import Debug.Trace (trace)
import Lib (generateCodeSet, intToCode, playMastermind)

debug :: Show a => a -> a
debug x = trace ("Debug: " ++ show x) x

{- Bind Int to readLn -}
readInt :: IO Int
readInt = readLn

{- Bind String to readLn -}
readStr :: IO String
readStr = readLn

main :: IO ()
main = do
    putStrLn "Game Configuration: How many colors?"
    numColors <- readInt -- Should do error handling
    putStrLn "Game Configuration: How many holes?"
    numHoles <- readInt
    putStrLn $
        "Config: " ++ show (numColors :: Int) ++ " colors and "
        ++ show (numHoles :: Int)
        ++ " holes"
    putStrLn "What is the solution code?"
    solStr <- readInt
    -- TODO: verify solStr length and valid symbols, convert to Code
    -- let sol = [1, 2, 1, 1]
    let sol = intToCode solStr
    let initialGuess = replicate numHoles 1 
    -- putStrLn $ "DEBUG NOTE: Using solution " ++ show sol ++ " instead"
    let codeset = generateCodeSet [1 .. numColors] numHoles
    num_turns_required <- playMastermind initialGuess sol 1 codeset codeset
    putStrLn $ "Solved in " ++ show num_turns_required ++ " turns"
