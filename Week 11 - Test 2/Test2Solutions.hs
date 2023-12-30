module Test2Solutions ( pentaFast
                      , statePenta
                      , deposit
                      , withdraw
                      , runAll
                      , circuit
                      , insert
                      , popMin
                      ) where

import Types (Heap(..),
              Circuit(..),
              Expr(..),
              Bin(..),
              BankAccount,
              Transaction(WithdrawalFailed, Deposit, DepositFailed, Withdrawal))
import Control.Monad.State (modify, runState, MonadState(put, get), State)

--------------------------------------------------------------------------------
-- QUESTION 1
--------------------------------------------------------------------------------

pentaFast :: Integer -> Integer
pentaFast n = a
  where
    ((),(a,b,c,d,e)) = runState (statePenta n) (0,1,2,3,4)

statePenta :: Integer -> State (Integer,Integer,Integer,Integer,Integer) ()
statePenta 0 = return ()
statePenta n = do
                 modify (\(a,b,c,d,e) -> (b,c,d,e,a+2*b-3*c+4*d-5*e))
                 statePenta (n-1)

--------------------------------------------------------------------------------
-- QUESTION 2
--------------------------------------------------------------------------------

deposit :: Int -> State BankAccount ()
deposit amount = do
    (balance, acclog) <- get
    if amount > 0 then
        do
            let newBalance = balance + amount
            put (newBalance, acclog ++ [Deposit])
    else
        do
            put (balance, acclog ++ [DepositFailed])

withdraw :: Int -> State BankAccount ()
withdraw amount = do
    (balance, acclog) <- get
    if amount > 0 && balance >= amount then
        do
            let newBalance = balance - amount
            put (newBalance, acclog ++ [Withdrawal])
    else
        do
            put (balance, acclog ++ [WithdrawalFailed])
--------------------------------------------------------------------------------
-- QUESTION 3
--------------------------------------------------------------------------------

runAll :: Monad m => Bin (m a) -> m (Bin a)
runAll Leaf         = pure Leaf
runAll (Node m l r) = do x <- m
                         l' <- runAll l
                         r' <- runAll r
                         pure (Node x l' r')

--------------------------------------------------------------------------------
-- QUESTION 4
--------------------------------------------------------------------------------

circuit :: Expr -> Circuit
circuit (Var     x    ) = Input x
circuit (Not     e    ) = Nand (circuit e) (circuit e)
circuit (Or      e1 e2) = circuit (Not (And (Not e1) (Not e2)))
circuit (Implies e1 e2) = circuit (Or (Not e1) e2)
circuit (And     e1 e2) = let e = Nand (circuit e1) (circuit e2) in Nand e e

--------------------------------------------------------------------------------
-- QUESTION 5
--------------------------------------------------------------------------------

insert :: Ord a => a -> Heap a -> Heap a
insert val Empty = HeapNode val Empty Empty
insert val (HeapNode x left right)
    | val <= x  = bubbleUp val (HeapNode x left right)
    | otherwise = HeapNode x left (insert val right)

bubbleUp :: Ord a => a -> Heap a -> Heap a
bubbleUp val Empty = HeapNode val Empty Empty
bubbleUp val (HeapNode x left right)
    | val <= x  = HeapNode val (bubbleUp x left) right
    | otherwise = HeapNode x left (bubbleUp val right)

popMin :: Ord a => Heap a -> (Maybe a, Heap a)
popMin Empty = (Nothing, Empty)
popMin (HeapNode x left right) = (Just x, merge left right)
    where
        merge Empty right = right
        merge left Empty  = left
        merge (HeapNode x1 l1 r1) (HeapNode x2 l2 r2)
            | x1 <= x2  = HeapNode x1 (merge l1 r1) (HeapNode x2 l2 r2)
            | otherwise = HeapNode x2 (HeapNode x1 l1 r1) (merge l2 r2)
