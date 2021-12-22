module Lib
    (
        parseCode,
        generateCodeSet,
        guessResult,
        filterCodeSet,
        scoreGuess,
        playMastermind,
        playMastermindParMap,
        playMastermindChunkStrategy,
        playMastermindShrinkingChunks,
        playMastermindGrowingChunks
    ) where
import Data.List (isInfixOf, minimumBy, nub, foldl1')
import Data.Function (on)
import Control.Monad.Par (parMap, runPar)
import Control.Parallel.Strategies(using, parList, rseq)
import Debug.Trace (trace)


debug :: Show a => a -> a
debug x = trace ("Debug: " ++ show x) x


type ResponsePegs = (Int, Int) -- (#black, #white)

type Code = [Int]

type CodeSet = [Code] -- TODO: Use set instead of list for speed?

type Possibility = (Int, Bool, Code) -- (Score, Invalid, Code)


parseCode :: String -> Code
parseCode str = (map read $ words str) :: [Int]

generateCodeSet :: (Eq a, Num a) => [a] -> a -> [[a]]
generateCodeSet [] _ = error "Give me a non-empty list"
generateCodeSet list hole
    | hole == 1           = [ [x] | x <- list]
    | otherwise           = [ x:xs | x <- list, xs <- generateCodeSet list $ hole - 1]


guessResult :: Code -> Code -> ResponsePegs
guessResult ans guess = (numBlack, numWhite)
    where numBlack = length $ filter id $ zipWith (==) ans guess
          numWhite = sum (map minCodeCount $ nub guess) - numBlack
          minCodeCount v = min (count v ans) (count v guess)
          count v ls = length $ filter (==v) ls

{-
    Given the current codeset, a guess, and its corresponding response,
    return a new codeset with codes which are now impossible filtered out
-}
filterCodeSet :: CodeSet -> Code -> ResponsePegs -> CodeSet
filterCodeSet set guess response =
    filter ((response ==) . guessResult guess) set


scoreGuess :: CodeSet -> Code -> Possibility
scoreGuess possible code = (score, not valid, code)
    where
        valid = code `elem` possible
        allResponses = map (guessResult code) possible
        score = getMaxCount allResponses
        getMaxCount xs = foldl1' max $ map snd $ getCounts xs
        incCount o [] = [(o, 1)]
        incCount o (x@(v, c) : xs)
            | v == o = (v, c + 1) : xs
            | otherwise = x : incCount o xs
        getCounts xs = foldr incCount [] xs



playMastermind ::  Code -> Code -> Int -> CodeSet -> CodeSet -> IO Int
playMastermind guess solution k fullSet possibleSet = do
  putStrLn $ "Guessing: " ++ show guess
  let response@(blk, wht) = guessResult guess solution
  putStrLn $
    "Response: " ++ show blk ++ " black and "
      ++ show wht ++ " white"
  if blk == length guess then do
    putStrLn $ "Solved: " ++ show guess
    return k
  else do
    let possibleSet' = filterCodeSet possibleSet guess response
    let possibilities = map (scoreGuess possibleSet') fullSet
    let (_, _, nextGuess) = minimum possibilities
    playMastermind nextGuess solution (k + 1) fullSet possibleSet'



playMastermindParMap ::  Code -> Code -> Int -> CodeSet -> CodeSet -> IO Int
playMastermindParMap guess solution k fullSet possibleSet = do
  putStrLn $ "Guessing: " ++ show guess
  let response@(blk, wht) = guessResult guess solution
  putStrLn $
    "Response: " ++ show blk ++ " black and "
      ++ show wht ++ " white"
  if blk == length guess then do
    putStrLn $ "Solved: " ++ show guess
    return k
  else do
    let possibleSet' = filterCodeSet possibleSet guess response
    let possibilities = runPar $ parMap (scoreGuess possibleSet') fullSet
    let (_, _, nextGuess) = minimum possibilities
    playMastermindParMap nextGuess solution (k + 1) fullSet possibleSet'


splitToChunks :: Int -> [a] -> [[a]]
splitToChunks numChunks ls = chunk (length ls `quot` numChunks) ls
  where
    chunk _ [] = []
    chunk n ls = let (as, bs) = splitAt n ls in
      as : chunk n bs

bestFromChunk :: CodeSet -> CodeSet -> Possibility
bestFromChunk possibleSet chunk = foldl1' min $ map (scoreGuess possibleSet) chunk


optimizeChuck :: (Real a, Integral a) => a -> Float -> Int
optimizeChuck n d = round $ (realToFrac n) / (realToFrac d)


-- In chunks
playMastermindChunkStrategy ::  Int -> Code -> Code -> Int -> CodeSet -> CodeSet -> IO Int
playMastermindChunkStrategy numChunks guess solution k fullSet possibleSet = do
  putStrLn $ "Guessing: " ++ show guess
  let response@(blk, wht) = guessResult guess solution
  putStrLn $
    "Response: " ++ show blk ++ " black and "
      ++ show wht ++ " white"
  if blk == length guess then do
    putStrLn $ "Solved: " ++ show guess
    return k
  else do
    let possibleSet' = filterCodeSet possibleSet guess response
    let chunks = splitToChunks numChunks fullSet -- TODO: Tune the number of chunks
    let possibilities = map (bestFromChunk possibleSet') chunks `using` parList rseq
    let (_, _, nextGuess) = foldl1' min possibilities
    playMastermindChunkStrategy numChunks nextGuess solution (k + 1) fullSet possibleSet'



-- In chunks which get smaller on later turns
playMastermindShrinkingChunks ::  Int -> Code -> Code -> Int -> CodeSet -> CodeSet -> IO Int
playMastermindShrinkingChunks numChunks guess solution k fullSet possibleSet = do
  putStrLn $ "Guessing: " ++ show guess
  let response@(blk, wht) = guessResult guess solution
  putStrLn $
    "Response: " ++ show blk ++ " black and "
      ++ show wht ++ " white"
  if blk == length guess then do
    putStrLn $ "Solved: " ++ show guess
    return k
  else do
    let possibleSet' = filterCodeSet possibleSet guess response
    let chunks = splitToChunks numChunks fullSet -- TODO: Tune the number of chunks
    let possibilities = map (bestFromChunk possibleSet') chunks `using` parList rseq
    let (_, _, nextGuess) = foldl1' min possibilities
    playMastermindShrinkingChunks (debug (optimizeChuck numChunks 1.6)) nextGuess solution (k + 1) fullSet possibleSet'



-- In chunks which get smaller on later turns
playMastermindGrowingChunks ::  Int -> Code -> Code -> Int -> CodeSet -> CodeSet -> IO Int
playMastermindGrowingChunks numChunks guess solution k fullSet possibleSet = do
  putStrLn $ "Guessing: " ++ show guess
  let response@(blk, wht) = guessResult guess solution
  putStrLn $
    "Response: " ++ show blk ++ " black and "
      ++ show wht ++ " white"
  if blk == length guess then do
    putStrLn $ "Solved: " ++ show guess
    return k
  else do
    let possibleSet' = filterCodeSet possibleSet guess response
    let chunks = splitToChunks numChunks fullSet -- TODO: Tune the number of chunks
    let possibilities = map (bestFromChunk possibleSet') chunks `using` parList rseq
    let (_, _, nextGuess) = foldl1' min possibilities
    playMastermindGrowingChunks (numChunks * 2) nextGuess solution (k + 1) fullSet possibleSet'