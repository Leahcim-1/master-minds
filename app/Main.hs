module Main where

import Lib

readInt :: IO Int
readInt = readLn

main :: IO ()
main = do
    putStrLn "Game Configuration: How many colors?"
    numColors <- readInt-- Should do error handling
    putStrLn "Game Configuration: How many holes?"
    numHoles <- readInt
    putStrLn $ "Config: " ++ (show (numColors::Int)) ++ " colors and "
        ++ (show (numHoles::Int)) ++ " holes"
    putStrLn "What is the solution code?"
    solStr <- readInt
    -- TODO: verify solStr length and valid symbols, convert to Code
    let sol = [1, 2, 3, 3] :: Code Int -- TODO: remove
    let codeset = generateCodeSet [1..numColors] numHoles
    putStr "hi"
    -- TODO: step through and solve
