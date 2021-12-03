module Lib
    (
    ) where
import Text.XHtml.Transitional (HotLink(hotLinkAttributes))


data Peg = White | Black

type ResponsePegs = [Peg]

type Code a = [a]

type CodeSet a = [Code a]

generateCodeSet :: (Eq a, Num a) => [a] -> a -> [[a]]
generateCodeSet [] _ = error "Give me a non-empty list"
generateCodeSet list hole 
    | hole == 1           = [ [x] | x <- list]
    | otherwise           = [ x:xs | x <- list, xs <- generateCodeSet list 3]
    

guessResult :: Ord a => Code a -> Code a -> ResponsePegs
guessResult ans guess = error ""

generateNextGuess :: Ord a => CodeSet a -> ResponsePegs -> Code a
generateNextGuess codeset [] = error ""
generateNextGuess codeset res@(x:xs) = error ""




