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
    let sol = [1, 2, 1, 1] :: Code Int -- TODO: remove
    let initialGuess = [1, 1, 1, 1] :: Code Int -- TODO: try all
    putStrLn $ "DEBUG NOTE: Using solution " ++ (show sol) ++ " instead"
    let codeset = generateCodeSet [1..numColors] numHoles
    num_turns_required <- play_mastermind initialGuess sol 1 codeset codeset
    putStrLn $ "Solved in " ++ (show num_turns_required) ++ " turns"

play_mastermind :: (Ord a, Show a) => Code a -> Code a -> Int -> CodeSet a -> CodeSet a -> IO Int
play_mastermind guess solution k fullSet possibleSet = do
    -- TODOremove putStrLn $ "Debug (Remaining): " ++ (show $ length possibleSet)
    -- TODOremove putStrLn $ "Debug (Remaining): " ++ (show $ take 5 possibleSet)
    putStrLn $ "Guessing: " ++ (show guess)
    let response = guessResult guess solution
    putStrLn $ "Response: " ++ (show $ fst response) ++ " black and "
        ++ (show $ snd response) ++ " white"
    let possibleSet' = filterCodeSet possibleSet guess response
    -- TODOremove putStrLn $ "Debug (Response): " ++ (show response)
    -- TODOremove putStrLn $ "Debug (Remaining New): " ++ (show $ length possibleSet')
    -- TODOremove putStrLn $ "Debug (Remaining New): " ++ (show $ take 5 possibleSet')
    if length possibleSet' == 1
        then do
            putStrLn $ "Solved: " ++ (show $ head possibleSet')
            return (k + 1)
        else do
            let possibilities = map (scoreGuess possibleSet') fullSet
            -- TODOremove putStrLn $ "Debug (Possibilities): " ++ (show $ take 3 possibilities)
            let nextGuess = getThird $ minimum possibilities
            play_mastermind nextGuess solution (k+1) fullSet possibleSet'
    where
        getThird (_,_,x) = x
        scoreGuess possible code = (score, valid, code)
            where
                valid = isInfixOf [code] possible
                allResponses = map (guessResult code) possible
                score = getMaxCount allResponses
                getMaxCount xs = maximum $ map snd $ getCounts xs
                -- compareSnd a b = compare (snd a) (snd b)
                incCount o [] = [(o, 1)]
                incCount o (x@(v, c):xs)
                    | v == o = (v, c + 1) : xs
                    | otherwise = x : incCount o xs
                getCounts xs = foldr incCount [] xs