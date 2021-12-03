module Lib
    ( 
    ) where

import Data.List(permutations)


data Peg = White | Black

type ResponsePegs = [Peg]

type Code a = [a]

type CodeSet a = [Code a] 

generateCodeSet :: Ord a => [a] -> CodeSet a
generateCodeSet [] = error "Give me a non-empty list"
generateCodeSet list = permutations list --  Change to own solutions 

guessResult :: Ord a => Code a -> Code a -> ResponsePegs
guessResult ans guess = error ""

generateNextGuess :: Ord a => CodeSet a -> Code a
generateNextGuess codeset = error ""




