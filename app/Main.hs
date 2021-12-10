module Main where

import Lib
import Data.List(isInfixOf, maximumBy)

{- Bind Int to readLn -}
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
    let initialGuess = [1, 1, 1, 1] :: Code Int -- TODO: try all
    let codeset = generateCodeSet [1..numColors] numHoles
    -- TODO: step through and solve
    num_turns_required <- play_mastermind initialGuess sol 1 codeset codeset
    putStrLn $ "Solved in " ++ (show num_turns_required) ++ " turns"

play_mastermind :: Code a -> Code a -> Int -> CodeSet a -> CodeSet a -> IO Int
play_mastermind guess solution k fullSet possibleSet = do
    putStrLn $ "Guessing: " ++ (show guess)
    let response = guessResult guess solution
    let possibleSet' = filterCodeSet possibleSet guess response
    if length possibleSet' == 1
        then do
            putStrLn $ "Solved: " ++ (show $ head possibleSet')
            return k
        else do
            let possibilities = map (scoreGuess possibleSet) fullSet
            let nextGuess = getThird $ minimum possibilities
            play_mastermind nextGuess solution (k+1) fullSet possibleSet'
    where
        getThird (_,_,x) = x
        scoreGuess possible code = (score, valid, code)
            where
                valid = isInfixOf [code] possible
                allResponses = map (guessResult code) possible
                score = getMaxCount allResponses
                getMaxCount xs = maximumBy compareSnd $ getCounts xs
                compareSnd a b = compare (snd a) (snd b)
                incCount o [] = [(o, 1)]
                incCount o (x@(v, c):xs)
                    | v == o = (v, c + 1) : xs
                    | otherwise = x : incCount o xs
                getCounts xs = foldr incCount [] xs