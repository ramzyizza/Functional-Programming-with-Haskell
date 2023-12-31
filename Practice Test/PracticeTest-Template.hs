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
checkParity = undefined

{- Question 2 -}

substitution :: String -> String -> String
substitution plaintext key = undefined

{- Question 3 -}

largestPrimeBetween :: Int -> Int
largestPrimeBetween = undefined

strongPrimes :: Int -> [Int]
strongPrimes n = undefined

{- Question 4 -}

executeCommands :: [Command] -> (Int, Int) -> (Int, Int)
executeCommands = undefined

{- Question 5 -}

babylonianPalindromes :: [Integer]
babylonianPalindromes = undefined
