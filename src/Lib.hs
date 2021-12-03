module Lib
    ( 
    ) where



data Peg = White | Black

type ResponsePegs = [Peg]

type Code a = [a]

type CodeSet a = [Code a] -- S

generateCodeSet :: Ord a => [a] -> CodeSet a
generateCodeSet list = error ""

guessResult :: Ord a => Code a -> Code a -> ResponsePegs
guessResult ans guess = error ""

generateNextGuess :: Ord a => CodeSet a -> Code a
generateNextGuess codeset = error ""

