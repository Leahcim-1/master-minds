module Main where

import Lib

main :: IO ()
main = do
    putStrLn "Game Configuration: How many colors?"
    numColors <- readLn -- Should do error handling
    putStrLn "Game Configuration: How many holes?"
    numHoles <- readLn
    putStrLn $ "Config: " ++ (show (numColors::Int)) ++ " colors and "
        ++ (show (numHoles::Int)) ++ " holes"
    putStrLn "What is the solution code?"
    solStr <- readLn
    -- TODO: verify solStr length and valid symbols, convert to Code
    let sol = [1, 2, 3, 3] :: Code Int -- TODO: remove
    let codeset = generateCodeSet [1..numColors]
    -- TODO: step through and solve
