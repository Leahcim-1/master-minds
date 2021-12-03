module Lib
    ( 
    ) where

import Data.List



data Peg = White | Black

instance Show Peg where
    show Black = "Black"
    show White = "White"

type ResponsePegs = [Peg]

type Code a = [a]

type CodeSet a = [Code a] -- S

generateCodeSet :: Ord a => [a] -> CodeSet a
generateCodeSet list = error ""

guessResult :: Ord a => Code a -> Code a -> ResponsePegs
guessResult ans guess = replicate numBlack Black ++ replicate numWhite White
    where numBlack = length $ filter id $ zipWith (==) ans guess
          numWhite = sum (map minCodeCount $ nub guess) - numBlack
          minCodeCount v = min (count v ans) (count v guess)
          count v ls = length $ filter (==v) ls

generateNextGuess :: Ord a => CodeSet a -> Code a
generateNextGuess codeset = error ""

