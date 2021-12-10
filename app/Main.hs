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
    let initialGuess = [1, 1, 1, 1] :: Code Int -- TODO: try all
    let codeset = generateCodeSet [1..numColors]
    -- TODO: step through and solve
    let num_turns_required = play_mastermind initialGuess sol 1 codeset codeset
    putStrLn $ "Solved in " ++ num_turns_required ++ " turns"

play_mastermind :: Code a -> Code a -> CodeSet a -> CodeSet a -> IO Int
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
            let nextGuess = minimum possibilities
            play_mastermind nextGuess solution (k+1) fullSet possibleSet'
    where
        scoresGuess possible code = (score, valid, code)
        valid = isInfixOf [code] possible
        allResponses = map (guessResult code) valid
        score = maximum getCounts allResponses
        getMaxCount xs = maximumBy compareSnd getCounts xs
        compareSnd a b = compare (snd a) (snd b)
        incCount o [] = [(o, 1)]
        incCount o ((v, c):xs)
            | v == o = (v, c + 1)
            | otherwise = x : incCount o xs
        getCounts xs = foldr incCount [] xs