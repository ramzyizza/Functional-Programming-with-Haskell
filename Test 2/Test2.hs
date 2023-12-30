-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Test2 ( pentaFast
             , statePenta
             , deposit
             , withdraw
             , runAll
             , circuit
             , insert
             , popMin
             ) where

import Types

import Control.Monad.State

{- QUESTION 1 -}

pentaFast :: Integer -> Integer
pentaFast n = a
  where
    ((),(a,b,c,d,e)) = runState (statePenta n) (0,1,2,3,4)

statePenta :: Integer -> State (Integer,Integer,Integer,Integer,Integer) ()
statePenta = undefined


{- QUESTION 2 -}

transactions1, transactions2 :: State BankAccount ()
transactions1 = do 
    deposit 1000
    deposit (-100)
    deposit 500
    deposit 300

transactions2 = do
    deposit 500
    withdraw 700
    deposit 100
    withdraw 1200

updateAccount :: Int -> Transaction -> State BankAccount ()
updateAccount amount transaction = do
                                    (balance, transactions) <- get
                                    put (balance + amount, transactions ++ [transaction])

deposit :: Int -> State BankAccount ()
deposit amount
              | amount > 0 = updateAccount amount Deposit
              | otherwise  = updateAccount 0 DepositFailed

withdraw :: Int -> State BankAccount ()
withdraw amount = do
                (balance, transactions) <- get
                let transactionType = if amount > 0 && amount <= balance then Withdrawal else WithdrawalFailed
                let balanceChange = if transactionType == Withdrawal then -amount else 0
                updateAccount balanceChange transactionType

{- QUESTION 3 -}

runAll :: Monad m => Bin (m a) -> m (Bin a)
runAll Leaf         = return Leaf
runAll (Node m left right) = do
                    dat <- m
                    left' <- runAll left
                    right' <- runAll right
                    return (Node dat left' right')

{- QUESTION 4 -}

applyNand :: Circuit -> Circuit -> Circuit
applyNand c1 c2 = Nand c1 c2

applyNot :: Circuit -> Circuit
applyNot a = applyNand a a

circuit :: Expr -> Circuit
circuit (Var c)         = Input c
circuit (Not expr)      = applyNot (circuit expr)
circuit (And e1 e2)     = applyNot (applyNand (circuit e1) (circuit e2))
circuit (Or e1 e2)      = let negatedE1 = applyNot (circuit e1)
                              negatedE2 = applyNot (circuit e2)
                          in applyNot (applyNand (applyNand negatedE1 negatedE2) (applyNand negatedE1 negatedE2))
circuit (Implies e1 e2) = applyNot (applyNand (applyNot (circuit e1)) (applyNand (applyNot (circuit e1)) (circuit e2)))

{- QUESTION 5 -}

insert :: Ord a => a -> Heap a -> Heap a
insert x Empty = HeapNode x Empty Empty
insert x heap = insertHelper x heap
  where
    insertHelper newVal Empty = HeapNode newVal Empty Empty
    insertHelper newVal (HeapNode rootVal left right)
        | newVal < rootVal = goUp newVal rootVal left right
        | otherwise        = HeapNode rootVal left (insertHelper newVal right)

goUp :: Ord a => a -> a -> Heap a -> Heap a -> Heap a
goUp inserted root left right = HeapNode inserted (insert root left) right

popMin :: Ord a => Heap a -> (Maybe a, Heap a)
popMin Empty = (Nothing, Empty)
popMin (HeapNode x left right) = (Just x, heap left right)
  where
    heap Empty r = r
    heap l Empty = l
    heap (HeapNode lv ll lr) (HeapNode rv rl rr)
        | lv < rv   = HeapNode lv (heap ll lr) (HeapNode rv rl rr)
        | otherwise = HeapNode rv (HeapNode lv ll lr) (heap rl rr)


