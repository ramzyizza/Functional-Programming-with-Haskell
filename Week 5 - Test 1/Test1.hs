-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Test1 ( evenMajority
             , get5SmoothNumbers
             , comesBefore
             , countApplications
             , f 
             ) where

import Types

{- QUESTION 1 -}

evenMajority :: [Int] -> Bool
evenMajority ns = let countEven  = length (filter even ns)
                      countTotal = length ns
                  in  countEven * 2 > countTotal

{- QUESTION 2 -}

get5SmoothNumbers :: Int -> [Int]
get5SmoothNumbers k = helper 1 k
                  where
                    helper x k | x > k = []
                               | check5Smooth x = x : helper (x + 1) k
                               | otherwise = helper (x + 1) k
                    check5Smooth x = all (\y -> y == 5 || y == 3 || y == 2) (primeFactors' x)

factors' :: Int -> [Int]
factors' n = [ k | k <- [1..n] , n `mod` k == 0 ]

isPrime' :: Int -> Bool
isPrime' n = factors' n == [1, n]

primeFactors' :: Int -> [Int]
primeFactors' n = [x | x <- [1..n], n `mod` x == 0 && isPrime' x]

{- QUESTION 3 -}

comesBefore :: TrainStop -> TrainStop -> Bool
comesBefore station1 station2 | station1 == station2 = False
                              | station2 == BirminghamNewStreet = True
                              | station1 == BirminghamNewStreet = False
                              | otherwise = helper station1 station2
                              where
                                helper :: TrainStop -> TrainStop -> Bool
                                helper current dest | current == dest = True
                                                    | current == FiveWays = False
                                                    | otherwise = helper (theStopAfter current) dest
                                                    
{- QUESTION 4 -}

countApplications :: (a -> a) -> (a -> Bool) -> a -> Int
countApplications f p x = count 0 x
                        where
                        count n y | p y       = n
                                  | otherwise = count (n + 1) (f y)

{- QUESTION 5 -}

f :: (a -> a -> r) -> ((a -> r) -> a) -> r
f a b = a x (b (\y -> a y x))
  where x = b (\y -> a y y)



