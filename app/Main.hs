module Main where

import Data.Function (on)
import Data.List (isInfixOf, minimumBy, find)
import Debug.Trace (trace)
import Lib
import Data.Maybe (isJust)

debug :: Show a => a -> a
debug x = trace ("Debug: " ++ show x) x

{- Bind Int to readLn -}
readInt :: IO Int
readInt = readLn

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
    solStr <- getLine
    -- TODO: verify solStr length and valid symbols, convert to Code
    -- let sol = [1, 2, 1, 1]
    let sol = parseCode solStr
    if (length sol /= numHoles)
        || isJust (find (\x -> x > numColors || x < 0) sol) then -- TODO: Replace isJust with "any" fold
            putStrLn "Invalid input code!"
    else do
        let initialGuess = replicate numHoles 1 
        -- putStrLn $ "DEBUG NOTE: Using solution " ++ show sol ++ " instead"
        let codeset = generateCodeSet [1 .. numColors] numHoles
        num_turns_required <- playMastermindChunkStrategy 1024 initialGuess sol 1 codeset codeset
        -- num_turns_required <- playMastermindParMap initialGuess sol 1 codeset codeset
        putStrLn $ "Solved in " ++ show num_turns_required ++ " turns"
