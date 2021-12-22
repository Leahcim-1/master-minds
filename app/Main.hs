module Main where

import Data.Function (on)
import Data.List (isInfixOf, minimumBy, find)
import Debug.Trace (trace)
import Lib
import Data.Maybe (isJust)

debug :: Show a => a -> a
debug x = trace ("Debug: " ++ show x) x

{- Bind Int to readLn -}
readPosInt :: IO Int
readPosInt = do
    val <- readLn
    if val > 0 then
        return val
    else
        error "Input must be a positive integer"

main :: IO ()
main = do
    putStrLn "Game Configuration: How many colors?"
    numColors <- readPosInt
    putStrLn "Game Configuration: How many holes?"
    numHoles <- readPosInt
    putStrLn $
        "Config: " ++ show (numColors :: Int) ++ " colors and "
        ++ show (numHoles :: Int)
        ++ " holes"
    putStrLn "What is the solution code?"
    solStr <- getLine
    let sol = parseCode solStr
    if (length sol /= numHoles)
        || any (\x -> x > numColors || x <= 0) sol then
            putStrLn "Invalid input code!"
    else do
        let initialGuess = replicate numHoles 1 
        let codeset = generateCodeSet [1 .. numColors] numHoles
        num_turns_required <- playMastermindChunkStrategy 32 initialGuess sol 1 codeset codeset
        -- num_turns_required <- playMastermindParMap initialGuess sol 1 codeset codeset
        putStrLn $ "Solved in " ++ show num_turns_required ++ " turns"
