-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module PracticeTest ( checkParity
                    , substitution
                    , largestPrimeBetween
                    , strongPrimes
                    , executeCommands
                    , babylonianPalindromes) where

import Types

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 1 -}

checkParity :: String -> Bool
checkParity bit = all even1 (chop 8 bit) && mod8 bit
                where
                    chop :: Int -> [a] -> [[a]]
                    chop size xs = if length xs < size then [] else take size xs : chop size (drop size xs)

mod8 :: String -> Bool
mod8 bit = (length bit) `mod` 8 == 0

checkEven :: Int -> Bool
checkEven bit = bit `mod` 2 == 0

counter1 :: String -> Int
counter1 = foldl (\ys y -> if y == '1' then ys + 1 else ys) 0

even1 :: String -> Bool
even1 bit = checkEven(counter1 bit)

{- Question 2 -}

substitution :: String -> String -> String
substitution plaintext key = [cipher char key | char <- plaintext]

cipher :: Char -> String -> Char
cipher char key | isLetter char == False = char
                 | isLower char  == True  = toLower(key !! charLabel char)
                 | otherwise = key !! charLabel char
                 
{- Question 3 -}

largestPrimeBetween :: Int -> Int
largestPrimeBetween num = maximum [ x | x <- range num, checkPrime x]

checkPrime :: Int -> Bool
checkPrime num = if num < 0 then False else all (\x -> num `mod` x /= 0) [2..num-1]

range :: Int -> [Int]
range x = [x' | x' <- [(x+1) .. (2*x)]]

strongPrimes :: Int -> [Int]
strongPrimes n = undefined
 
{- Question 4 -}

-- type Direction = Int
-- type Command = (Direction, Int)

executeCommands :: [Command] -> (Int, Int) -> (Int, Int)
executeCommands = undefined


{- Question 5 -}

babylonianPalindromes :: [Integer]
babylonianPalindromes = filter checkBabylonianPalindromes [0..]

int2Base60 :: Integer -> [Integer]
int2Base60 0 = []
int2Base60 num = int2Base60 (num `div` 60) ++ [num `mod` 60]

checkPalindrome :: [Integer] -> Bool
checkPalindrome xs = reverse xs == xs

checkBabylonianPalindromes :: Integer -> Bool
checkBabylonianPalindromes num = if length (int2Base60 num) > 1 then checkPalindrome (int2Base60 num) else False