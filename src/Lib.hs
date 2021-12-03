module Lib
    (
    ) where
import Text.XHtml.Transitional (HotLink(hotLinkAttributes))
import Data.List(nub)


data Peg = White | Black

instance Show Peg where
    show Black = "Black"
    show White = "White"

type ResponsePegs = [Peg]

type Code a = [a]

type CodeSet a = [Code a]

generateCodeSet :: (Eq a, Num a) => [a] -> a -> [[a]]
generateCodeSet [] _ = error "Give me a non-empty list"
generateCodeSet list hole 
    | hole == 1           = [ [x] | x <- list]
    | otherwise           = [ x:xs | x <- list, xs <- generateCodeSet list $ hole - 1]
    

guessResult :: Ord a => Code a -> Code a -> ResponsePegs
guessResult ans guess = replicate numBlack Black ++ replicate numWhite White
    where numBlack = length $ filter id $ zipWith (==) ans guess
          numWhite = sum (map minCodeCount $ nub guess) - numBlack
          minCodeCount v = min (count v ans) (count v guess)
          count v ls = length $ filter (==v) ls

{-
    Given the current codeset, a guess, and its corresponding response,
    return a new codeset with codes which are now impossible filtered out
-}
filterCodeSet :: Ord a => CodeSet a -> Code a -> ResponsePegs -> CodeSet a
filterCodeSet set guess response = error ""

generateNextGuess :: Ord a => CodeSet a -> ResponsePegs -> Code a
generateNextGuess codeset [] = error ""
generateNextGuess codeset res@(x:xs) = error ""




