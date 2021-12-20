module Lib
    (
        intToCode,
        generateCodeSet,
        guessResult,
        filterCodeSet,
        generateNextGuess,
        compareCode,
        minimumPossibility,
        scoreGuess,
        playMastermind
    ) where
import Data.List (isInfixOf, minimumBy, nub)
import Data.Function (on)

import Debug.Trace (trace)


debug :: Show a => a -> a
debug x = trace ("Debug: " ++ show x) x


type ResponsePegs = (Int, Int) -- (#black, #white)

type Code = [Int]

type CodeSet = [Code] -- TODO: Use set instead of list for speed?

type Possibility = (Int, Bool, Code) -- (Score, Invalid, Code)


intToCode :: Int -> Code
intToCode x = reverse $ intToCode' x
    where intToCode' x'   
            | x' == 0    = []
            | otherwise = r : intToCode' q
            where (q, r) = x' `divMod` 10

generateCodeSet :: (Eq a, Num a) => [a] -> a -> [[a]]
generateCodeSet [] _ = error "Give me a non-empty list"
generateCodeSet list hole
    | hole == 1           = [ [x] | x <- list]
    | otherwise           = [ x:xs | x <- list, xs <- generateCodeSet list $ hole - 1]


guessResult :: Code -> Code-> ResponsePegs
guessResult ans guess = (numBlack, numWhite)
    where numBlack = length $ filter id $ zipWith (==) ans guess
          numWhite = sum (map minCodeCount $ nub guess) - numBlack
          minCodeCount v = min (count v ans) (count v guess)
          count v ls = length $ filter (==v) ls

{-
    Given the current codeset, a guess, and its corresponding response,
    return a new codeset with codes which are now impossible filtered out
    (TODONOTE: Can use this both for keeping track of valid final solutions and also to
    evaluate guesses given some possible response; use numBlack and first part of numWhite above to
    do this filtering)
-}
filterCodeSet :: CodeSet -> Code -> ResponsePegs -> CodeSet
filterCodeSet set guess response =
    filter ((response ==) . guessResult guess) set

generateNextGuess :: CodeSet -> ResponsePegs -> Code
generateNextGuess codeset resp = error ""


compareCode :: Code -> Code -> Ordering
compareCode [] [] = EQ
compareCode [x] [y]
    | x < y     = LT
    | x > y     = GT
    | x == y    = EQ
compareCode codeA@(x: xs) codeB@(y: ys)
    | cmp == EQ = compareCode xs ys
    | otherwise = cmp
    where cmp = compareCode [x] [y]
compareCode _ _ = error "Unmatch length of code"


minimumPossibility :: [Possibility ] -> Possibility
minimumPossibility p
    | length minLists == 1      = head minLists
    | length validMinLists == 1 = head validMinLists
    | null validMinLists        = smallTieBreaker p
    | otherwise                 = smallTieBreaker validMinLists
    where
        getThird (_, _, x)      = x
        (minScore, _, _)        = minimum p
        minLists                = filter (\(score, _, _) -> minScore == score) p
        validMinLists           = validTieBreaker minLists
        validTieBreaker list    = filter (\(_, v, _) -> v) $ debug list
        smallTieBreaker list    = minimumBy (\(_, _, codeA) (_, _, codeB) -> compareCode codeA codeB) list


scoreGuess :: [Code] -> Code -> Possibility
scoreGuess possible code = (score, valid, code)
    where
        valid = code `elem` possible
        allResponses = map (guessResult code) possible
        score = getMaxCount allResponses
        getMaxCount xs = maximum $ map snd $ getCounts xs
        -- compareSnd a b = compare (snd a) (snd b)
        incCount o [] = [(o, 1)]
        incCount o (x@(v, c) : xs)
            | v == o = (v, c + 1) : xs
            | otherwise = x : incCount o xs
        getCounts xs = foldr incCount [] xs


playMastermind ::  Code -> Code -> Int -> CodeSet -> CodeSet -> IO Int
playMastermind guess solution k fullSet possibleSet = do
  -- TODOremove putStrLn $ "Debug (Remaining): " ++ (show $ length possibleSet)
  -- TODOremove putStrLn $ "Debug (Remaining): " ++ (show $ take 5 possibleSet)
  putStrLn $ "Guessing: " ++ show guess
  let response = guessResult guess solution
  putStrLn $
    "Response: " ++ show (fst response) ++ " black and "
      ++ show (snd response)
      ++ " white"
  let possibleSet' = filterCodeSet possibleSet guess response
  -- TODOremove putStrLn $ "Debug (Response): " ++ (show response)
  -- TODOremove putStrLn $ "Debug (Remaining New): " ++ (show $ length possibleSet')
  -- TODOremove putStrLn $ "Debug (Remaining New): " ++ (show $ take 5 possibleSet')
  if length possibleSet' == 1
    then do
      putStrLn $ "Solved: " ++ show (head possibleSet')
      return (k + 1)
    else do
      let possibilities = map (scoreGuess possibleSet') fullSet
      -- TODOremove putStrLn $ "Debug (Possibilities): " ++ (show $ take 3 possibilities)
      let nextGuess = getThird $ minimum possibilities
      playMastermind nextGuess solution (k + 1) fullSet possibleSet'
  where
    getThird (_, _, x) = x