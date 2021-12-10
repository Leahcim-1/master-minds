module Lib
    (
    ) where
import Data.List(nub)


type ResponsePegs = (Int, Int) -- (#black, #white)

type Code a = [a]

type CodeSet a = [Code a]

generateCodeSet :: (Eq a, Num a) => [a] -> a -> [[a]]
generateCodeSet [] _ = error "Give me a non-empty list"
generateCodeSet list hole 
    | hole == 1           = [ [x] | x <- list]
    | otherwise           = [ x:xs | x <- list, xs <- generateCodeSet list $ hole - 1]
    

guessResult :: Ord a => Code a -> Code a -> ResponsePegs
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
filterCodeSet :: Ord a => CodeSet a -> Code a -> ResponsePegs -> CodeSet a
filterCodeSet set guess response =
    filter ((response ==) . guessResult guess) set

generateNextGuess :: Ord a => CodeSet a -> ResponsePegs -> Code a
generateNextGuess codeset resp = error ""




