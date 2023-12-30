{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-name-shadowing #-}
{-# LANGUAGE StandaloneDeriving, InstanceSigs #-}

module Test2TestCases where

import Data.Functor.Identity (Identity(..))
import Data.List             ((\\), delete, nub, intercalate)
import Data.Maybe            (isNothing, listToMaybe, catMaybes)
import Control.Monad         (replicateM)
import Control.Monad.Writer  (Writer, execWriter, tell)
import Control.Monad.State   (execState)

import TestCasesUtils  ((~=), permOf )
import Control.DeepSeq (NFData(..), deepseq)

import Test.QuickCheck (Gen,
                        Arbitrary(..),
                        Property,
                        Testable(property),
                        frequency,
                        vectorOf,
                        chooseInt,
                        chooseInteger,
                        elements,
                        counterexample,
                        property,
                        (==>),
                        sized,
                        (.&&.))

import           Types
import qualified Test2 as Student
import qualified Test2Solutions as Solutions


--------------------------------------------------------------------------------
-- Utilities for string formatting
--------------------------------------------------------------------------------

singleQuotes :: String -> String
singleQuotes s = "'" ++ s ++ "'"

indented :: String -> String
indented s = "\n\n    " ++ s ++ "\n\n"

-------------------------------------------------------------------------------
-- Tests: Question 1
-------------------------------------------------------------------------------

arbitraryLarge :: Gen Integer
arbitraryLarge = chooseInteger (100, 1000)

prop_pentaFast_base0 :: Property
prop_pentaFast_base0 =
  Student.pentaFast 0 ~= 0

prop_pentaFast_base1 :: Property
prop_pentaFast_base1 =
  Student.pentaFast 1 ~= 1

prop_pentaFast_base2 :: Property
prop_pentaFast_base2 =
  Student.pentaFast 2 ~= 2

prop_pentaFast_base3 :: Property
prop_pentaFast_base3 =
  Student.pentaFast 3 ~= 3

prop_pentaFast_base4 :: Property
prop_pentaFast_base4 =
  Student.pentaFast 4 ~= 4

prop_pentaFast_recursive :: Integer -> Property
prop_pentaFast_recursive n =
  counterexample msg (deepseq stu stu == deepseq sol sol)
  where
    msg :: String
    msg = unwords
      [ "Your output was " ++ show stu ++ ", but assuming you got the correct"
      , "output for the smaller inputs, we expected the output", show sol ++ "."
      ]

    stu :: Integer
    stu = Student.pentaFast n

    sol :: Integer
    sol =     student5
        + 2 * student4
        - 3 * student3
        + 4 * student2
        - 5 * student1

    student5 = Student.pentaFast (n - 5) :: Integer
    student4 = Student.pentaFast (n - 4) :: Integer
    student3 = Student.pentaFast (n - 3) :: Integer
    student2 = Student.pentaFast (n - 2) :: Integer
    student1 = Student.pentaFast (n - 1) :: Integer

-------------------------------------------------------------------------------
-- Tests: Question 2: TODO
-------------------------------------------------------------------------------

instance NFData Transaction where

  rnf :: Transaction -> ()
  rnf Deposit          = ()
  rnf Withdrawal       = ()
  rnf DepositFailed    = ()
  rnf WithdrawalFailed = ()

arbitraryBalance :: Gen CurrBalance
arbitraryBalance = chooseInt (0, 2500)

instance Arbitrary Transaction where
  arbitrary :: Gen Transaction
  arbitrary = elements [Deposit, Withdrawal, DepositFailed, WithdrawalFailed]

arbitraryTransactionList :: Gen [Transaction]
arbitraryTransactionList = do
  k <- chooseInt (0, 55)
  vectorOf k arbitrary

arbitraryBankAccount :: Gen BankAccount
arbitraryBankAccount = do
  b  <- arbitraryBalance
  ts <- arbitraryTransactionList
  return (b, ts)

shrinkBankAccount :: BankAccount -> [BankAccount]
shrinkBankAccount (b, ts) = [ (b, ts') | ts' <- shrink ts ]


arbitraryNonPositiveAmountAndBankAccount :: Gen (Int, BankAccount)
arbitraryNonPositiveAmountAndBankAccount = do (b, ts) <- arbitraryBankAccount
                                              n       <- chooseInt (-57, 0)
                                              return (n, (b, ts))

shrinkAmountAndBankAccount :: (Int, BankAccount)
                           -> [(Int, BankAccount)]
shrinkAmountAndBankAccount (n , ba) =
  [ (n, ba') | ba' <- shrinkBankAccount ba ]

arbitraryNonZeroAmountAndBankAccount :: Gen (Int, BankAccount)
arbitraryNonZeroAmountAndBankAccount = do
  ba <- arbitraryBankAccount
  n  <- chooseInt (1, 2700)
  return (n, ba)

prop_depositInvalidNoBalanceUpdate :: Int -> BankAccount -> Property
prop_depositInvalidNoBalanceUpdate n (b0, ts0) =
  let
    stu'    = execState (Student.deposit n) (b0, ts0)
    (b1, _) = deepseq stu' stu'
    msg     = unwords
                [ "On bank account " ++ singleQuotes (show (b0, ts0)) ++ ","
                , "expected balance"
                , "after running " ++ singleQuotes ("deposit " ++ show n)
                , "was"
                , singleQuotes (show b0)
                , "but your 'deposit' gave balance"
                , singleQuotes (show b1) ++ "."
                ]
  in
    if b1 == b0 then
      property True
    else
      counterexample msg False

deriving instance Ord Transaction

prop_depositInvalidFailLog :: Int -> BankAccount -> Property
prop_depositInvalidFailLog n (b0, ts0) =
  let
    stu'      = execState (Student.deposit n) (b0, ts0)
    (_, ts1)  = deepseq stu' stu'
    sol       = ts0 ++ [DepositFailed]
    msg       = unwords
                  [  "On bank account " ++ singleQuotes (show (b0, ts0)) ++ ","
                  , "expected transactions"
                  , "after running " ++ singleQuotes ("deposit " ++ show n)
                  , "were"
                  , singleQuotes (show sol)
                  , "but your 'deposit' gave the transaction list"
                  , singleQuotes (show ts1) ++ "."
                  ]
  in
    if ts1 == deepseq sol sol then
      property True
    else
      counterexample msg False

prop_depositInvalidFailLogModComm :: Int -> BankAccount -> Property
prop_depositInvalidFailLogModComm n (b0, ts0) =
  let
    stu'      = execState (Student.deposit n) (b0, ts0)
    (_, ts1)  = deepseq stu' stu'
    sol       = ts0 ++ [DepositFailed]
    msg       = unwords
                  [ "On bank account " ++ singleQuotes (show (b0, ts0)) ++ ","
                  , "expected transactions"
                  , "after running " ++ singleQuotes ("deposit " ++ show n)
                  , "were (ignoring the order)"
                  , singleQuotes (show sol)
                  , "but your 'deposit' gave the transaction list"
                  , singleQuotes (show ts1) ++ "."
                  ]
  in
    if ts1 `permOf` sol then
      property True
    else
      counterexample msg False

prop_depositNonZeroUpdate :: Int -> BankAccount -> Property
prop_depositNonZeroUpdate n (b0, ts0) =
  let
    stu'     = execState (Student.deposit n) (b0, ts0)
    (b1, _)  = deepseq stu' stu'
    msg      = unwords
                 [ "On bank account " ++ singleQuotes (show (b0, ts0)) ++ ","
                 , "expected balance after running"
                 , singleQuotes ("deposit " ++ show n)
                 , "was"
                 , singleQuotes
                     (show b0 ++ "+" ++ show n ++ "=" ++ show (b0 + n))
                 , "but your deposit gave balance"
                 , singleQuotes (show b1) ++ "."
                 ]
  in
    if b1 == b0 + n then
      property True
    else
      counterexample msg False

prop_depositNonZeroLog :: Int -> BankAccount -> Property
prop_depositNonZeroLog n (b0, ts0) =
  let
    stu'     = execState (Student.deposit n) (b0, ts0)
    (_, ts1) = deepseq stu' stu'
    sol      = deepseq (ts0 ++ [Deposit]) (ts0 ++ [Deposit])
    msg      = unwords
                  [ "On bank account " ++ singleQuotes (show (b0, ts0)) ++ ","
                  , "expected transactions"
                  , "after running " ++ singleQuotes ("deposit " ++ show n)
                  , "were"
                  , singleQuotes (show sol)
                  , "but your 'deposit' gave"
                  , singleQuotes (show ts1) ++ "."
                  ]
  in
    if ts1 == ts0 ++ [Deposit] then
      property True
    else
      counterexample msg False

prop_depositNonZeroLogModComm :: Int -> BankAccount -> Property
prop_depositNonZeroLogModComm n (b0, ts0) =
  let
    stu'     = execState (Student.deposit n) (b0, ts0)
    (_, ts1) = deepseq stu' stu'
    sol      = deepseq (ts0 ++ [Deposit]) (ts0 ++ [Deposit])
    msg      = unwords
                 [ "On bank account " ++ singleQuotes (show (b0, ts0)) ++ ","
                 , "expected transactions"
                 , "after running " ++ singleQuotes ("deposit " ++ show n)
                 , "were (ignoring the order)"
                 , singleQuotes (show sol)
                 , "but your 'deposit' gave the transaction list"
                 , singleQuotes (show ts1) ++ "."
                 ]
  in
    if ts1 `permOf` sol then
      property True
    else
      counterexample msg False

arbitraryInvalidPair :: Gen (Int, BankAccount)
arbitraryInvalidPair = do
  b       <- chooseInt (0, 729)
  n       <- chooseInt (b+1, b+1+257)
  ts      <- arbitrary
  return (n, (b, ts))

prop_withdrawInvalidNoUpdate :: Int -> BankAccount -> Property
prop_withdrawInvalidNoUpdate n (b0, ts0) =
  (n > b0) ==>
    let
      stu'    = execState (Student.withdraw n) (b0, ts0)
      (b1, _) = deepseq stu' stu'
      msg     = unwords
                  [ "On bank account " ++ singleQuotes (show (b0, ts0)) ++ ","
                  , "expected balance"
                  , "after running " ++ singleQuotes ("withdraw " ++ show n)
                  , "was"
                  , singleQuotes (show b0)
                  , "but your 'withdraw' gave balance"
                  , singleQuotes (show b1) ++ "."
                  ]
    in
      if b1 == b0 then
        property True
      else
        counterexample msg False

prop_withdrawInvalidFailLog :: Int -> BankAccount -> Property
prop_withdrawInvalidFailLog n (b0, ts0) =
  (n > b0) ==>
    let
      stu'     = execState (Student.withdraw n) (b0, ts0)
      (_, ts1) = deepseq stu' stu'
      sol      = deepseq (ts0 ++ [WithdrawalFailed]) (ts0 ++ [WithdrawalFailed])
      msg      = unwords
                   [  "On bank account " ++ singleQuotes (show (b0, ts0)) ++ ","
                   , "expected transactions"
                   , "after running " ++ singleQuotes ("withdraw " ++ show n)
                   , "were"
                   , singleQuotes (show sol)
                   , "but your 'withdraw' gave the transaction list"
                   , singleQuotes (show ts1) ++ "."
                   ]
    in
      if ts1 == sol then
        property True
      else
        counterexample msg False

prop_withdrawInvalidFailLogModComm :: Int -> BankAccount -> Property
prop_withdrawInvalidFailLogModComm n (b0, ts0) =
  (n > b0) ==>
    let
      stu'     = execState (Student.withdraw n) (b0, ts0)
      (_, ts1) = deepseq stu' stu'
      sol      = deepseq (ts0 ++ [WithdrawalFailed]) (ts0 ++ [WithdrawalFailed])
      msg      = unwords
                   [  "On bank account " ++ singleQuotes (show (b0, ts0)) ++ ","
                   , "expected transactions"
                   , "after running " ++ singleQuotes ("withdraw " ++ show n)
                   , "were"
                   , singleQuotes (show sol)
                   , "but your 'withdraw' gave the transaction list"
                   , singleQuotes (show ts1) ++ "."
                   ]
    in
      if ts1 `permOf` (ts0 ++ [WithdrawalFailed]) then
        property True
      else
        counterexample msg False


arbitraryValidPair :: Gen (Int, BankAccount)
arbitraryValidPair = do
  (b, ts) <- arbitraryBankAccount
  n       <- chooseInt (1, b)
  return (n, (b, ts))

prop_withdrawValidUpdate :: Int -> BankAccount -> Property
prop_withdrawValidUpdate n (b0, ts0) =
  (n > 0 && n <= b0) ==>
    let
      stu'    = execState (Student.withdraw n) (b0, ts0)
      (b1, _) = deepseq stu' stu'
      msg     = unwords
                  [  "On bank account " ++ singleQuotes (show (b0, ts0)) ++ ","
                  , "expected balance after running"
                  , singleQuotes ("withdraw " ++ show n)
                  , "was"
                  , singleQuotes (show (b0-n))
                  , "but your 'withdraw' gave balance"
                  , singleQuotes (show b1) ++ "."
                  ]
    in
      if b1 == b0-n then
        property True
      else
        counterexample msg False

prop_withdrawValidLog :: Int -> BankAccount -> Property
prop_withdrawValidLog n (b0, ts0) =
  (n > 0 && n <= b0) ==>
    let
      stu'     = execState (Student.withdraw n) (b0, ts0)
      (_, ts1) = deepseq stu' stu'
      sol      = deepseq (ts0 ++ [Withdrawal]) (ts0 ++ [Withdrawal])
      msg      = unwords
                   [ "On bank account " ++ singleQuotes (show (b0, ts0)) ++ ","
                   , "expected transactions"
                   , "after running " ++ singleQuotes ("withdraw " ++ show n)
                   , "were"
                   , singleQuotes (show sol)
                   , "but your 'withdraw' gave"
                   , singleQuotes (show ts1) ++ "." ]
    in
      if ts1 == sol then
        property True
      else
        counterexample msg False

prop_withdrawValidLogModComm :: Int -> BankAccount -> Property
prop_withdrawValidLogModComm n (b0, ts0) =
  (n > 0 && n <= b0) ==>
    let
      stu'     = execState (Student.withdraw n) (b0, ts0)
      (_, ts1) = deepseq stu' stu'
      sol      = deepseq (ts0 ++ [Withdrawal]) (ts0 ++ [Withdrawal])
      msg      = unwords
                   [ "On bank account " ++ singleQuotes (show (b0, ts0)) ++ ","
                   , "expected transactions"
                   , "after running " ++ singleQuotes ("withdraw " ++ show n)
                   , "were"
                   , "(ignoring the order)"
                   , singleQuotes (show sol)
                   , "but your 'withdraw' gave"
                   , singleQuotes (show ts1) ++ "." ]
    in
      if ts1 `permOf` sol then
        property True
      else
        counterexample msg False

-------------------------------------------------------------------------------
-- Tests: Question 3
-------------------------------------------------------------------------------

arbitraryBin :: Gen a -> Gen (Bin a)
arbitraryBin gx = sized $ \n -> do
    k <- chooseInt (0, n)
    go k
  where
    go 0 = pure Leaf
    go n = do
        x     <- gx
        split <- chooseInt (0, n - 1)
        l     <- go split
        r     <- go (n - 1 - split)
        pure $ Node x l r

arbitraryBinIncreasing :: Gen (Bin Int)
arbitraryBinIncreasing = sized $ \n -> do
    k <- chooseInt (0, n)
    go k [0..]
  where
    go 0 _      = pure Leaf
    go n (x:xs) = do
        split <- chooseInt (0, n - 1)
        l     <- go split (take split xs)
        r     <- go (n - 1 - split) (drop split xs)
        pure $ Node x l r
    go _ _      = undefined -- this will never happen

shrinkBinIncreasing :: Bin Int -> [Bin Int]
shrinkBinIncreasing Leaf         = []
shrinkBinIncreasing (Node x l r) =
  l : r : [ Node x l' r' | (l', r') <- shrink (l, r) ]

instance Arbitrary a => Arbitrary (Bin a) where
  arbitrary = arbitraryBin arbitrary

  shrink Leaf         = []
  shrink (Node x l r) =
    l : r : [ Node x' l' r' | (x', l', r') <- shrink (x, l, r)]

arbitraryRecCase :: Arbitrary a => Gen (Bin (Identity a))
arbitraryRecCase = Node . Identity <$> arbitrary <*> arbitrary <*> arbitrary

shrinkRecCase :: Arbitrary a => Bin a -> [Bin a]
shrinkRecCase = filter isNode . shrink
  where
    isNode Leaf         = False
    isNode (Node _ _ _) = True

newtype WriterBinInt = WBI {unWBI :: Bin Int}

instance Show WriterBinInt where
  show (WBI Leaf) = "Leaf"
  show (WBI (Node n l r)) = "(Node (tell " ++ show n ++ ") " ++ show (WBI l) ++ " " ++ show (WBI r) ++ ")"

instance Arbitrary WriterBinInt where
  arbitrary = WBI <$> arbitraryBinIncreasing
  shrink = map WBI . shrinkBinIncreasing . unWBI

instance NFData a => NFData (Bin a) where
 rnf Leaf            = ()
 rnf (Node x l r) = rnf x `seq` rnf l `seq` rnf r

toMonad :: Monad m => Bin a -> Bin (m a)
toMonad Leaf         = Leaf
toMonad (Node x l r) = Node (pure x) (toMonad l) (toMonad r)


toWriter :: Bin a -> Bin (Writer [a] ())
toWriter Leaf         = Leaf
toWriter (Node x l r) = Node (tell [x]) (toWriter l) (toWriter r)

inorderRunAll :: Monad m => Bin (m a) -> m (Bin a)
inorderRunAll Leaf         = pure Leaf
inorderRunAll (Node m l r) = do l' <- inorderRunAll l
                                x  <- m
                                r' <- inorderRunAll r
                                pure (Node x l' r')

postorderRunAll :: Monad m => Bin (m a) -> m (Bin a)
postorderRunAll Leaf         = pure Leaf
postorderRunAll (Node m l r) = do l' <- postorderRunAll l
                                  r' <- postorderRunAll r
                                  x  <- m
                                  pure (Node x l' r')

prop_runAll_baseCase :: Property
prop_runAll_baseCase =
  Student.runAll Leaf ~= (Identity Leaf :: Identity (Bin Int))

prop_runAll_recursiveCase :: Bin (Identity Int) -> Property
prop_runAll_recursiveCase Leaf                    = property True
prop_runAll_recursiveCase (Node (Identity x) l r) =
  counterexample msg (deepseq stu stu == deepseq sol sol)
  where
    msg :: String
    msg = unwords
      [ "Your output was " ++ show stu ++ ", but assuming you got the correct"
      , "output for the two subtrees, we expected the output", show sol ++ "."
      ]

    sol :: Identity (Bin Int)
    sol = do
      l' <- Student.runAll l
      r' <- Student.runAll r
      pure (Node x l' r')

    stu :: Identity (Bin Int)
    stu = Student.runAll (Node (Identity x) l r)

prop_runAll_correctOrder :: WriterBinInt -> Property
prop_runAll_correctOrder (WBI t)
  | isPreorder  = property True
  | isInorder   = counterexample inorderMsg      False
  | isPostorder = counterexample postorderMsg    False
  | otherwise   = counterexample unknownOrderMsg False
  where
    inorderMsg :: String
    inorderMsg = unwords
      [ "Your 'runAll' implementation seems to be executing the effects in the"
      , "binary tree in an in-order traversal, while it should be executing them"
      , "in a pre-order traversal."
      ]
    postorderMsg :: String
    postorderMsg = unwords
      [ "Your 'runAll' implementation seems to be executing the effects in the"
      , "binary tree in a post-order traversal, while it should be executing"
      , "them in a pre-order traversal."
      ]
    unknownOrderMsg :: String
    unknownOrderMsg = unwords
      [ "Your 'runAll' implementation seems to be executing the effects in the"
      , "binary tree in an arbitrary order, while it should be executing"
      , "them in a pre-order traversal."
      ]

    t' :: Bin (Writer [Int] ())
    t' = toWriter t

    student   = execWriter (Student.runAll t')   :: [Int]
    preorder  = execWriter (Solutions.runAll t') :: [Int]
    inorder   = execWriter (inorderRunAll t')    :: [Int]
    postorder = execWriter (postorderRunAll t')  :: [Int]

    isPreorder  = deepseq student student == deepseq preorder  preorder  :: Bool
    isInorder   = deepseq student student == deepseq inorder   inorder   :: Bool
    isPostorder = deepseq student student == deepseq postorder postorder :: Bool

-------------------------------------------------------------------------------
-- Tests: Question 4
-------------------------------------------------------------------------------

instance NFData Expr where

  rnf :: Expr -> ()
  rnf (Var     x)     = rnf x
  rnf (Not     e)     = rnf e
  rnf (And     e0 e1) = rnf e0 `seq` rnf e1
  rnf (Or      e0 e1) = rnf e0 `seq` rnf e1
  rnf (Implies e0 e1) = rnf e0 `seq` rnf e1

instance NFData Circuit where

  rnf :: Circuit -> ()
  rnf (Input x)    = rnf x
  rnf (Nand c0 c1) = rnf c0 `seq` rnf c1

arbitraryVar :: Gen Expr
arbitraryVar = do
  s <- elements ['a'..'f']
  return $ Var s

arbitraryVarPair :: Gen (Expr, Expr)
arbitraryVarPair = do
  e1 <- arbitraryVar
  e2 <- arbitraryVar
  return (e1 , e2)

sizedArbitraryExpr :: Int -> Gen Expr
sizedArbitraryExpr 0 = arbitraryVar
sizedArbitraryExpr n =
  let
    genNeg = do
      t <- sizedArbitraryExpr (n - 1)
      return (Not t)

    genCon :: (Expr -> Expr -> Expr) -> Gen Expr
    genCon c = do
      s <- sizedArbitraryExpr (n `div` 2)
      t <- sizedArbitraryExpr (n `div` 2)
      return (c s t)
  in
    frequency [ (3, arbitraryVar)
              , (2, genCon And)
              , (2, genCon Or)
              , (2, genCon Implies)
              , (2, genNeg) ]

instance Arbitrary Expr where

  arbitrary :: Gen Expr
  arbitrary = do
    n <- elements [1..7]
    sizedArbitraryExpr n

  shrink :: Expr -> [Expr]
  shrink (Var _)         = []
  shrink (Not e)         = e : shrink e
  shrink (And    e0 e1)  = e0 : e1 : [And e0' e1' | (e0' , e1') <- ss ]
                             where
                               ss = shrink (e0, e1)
  shrink (Or     e0 e1)  = e0 : e1 : [Or e0' e1' | (e0' , e1') <- ss ]
                             where
                               ss = shrink (e0, e1)
  shrink (Implies e0 e1) = e0 : e1 : [Implies e0' e1' | (e0' , e1') <- ss ]
                             where
                               ss = shrink (e0, e1)

-- The list of all variables mentioned in an expression.
vars :: Expr -> [Char]
vars (Var     c)     = [c]
vars (Not     e)     = vars e
vars (And     e1 e2) = vars e1 ++ vars e2
vars (Or      e1 e2) = vars e1 ++ vars e2
vars (Implies e1 e2) = vars e1 ++ vars e2

type Context = [Char]

arbitraryCtx :: Gen Context
arbitraryCtx = do
  l <- elements [1..15]
  vectorOf l (elements ['a'..'f'])

-- The Boolean function denoted by an expression. It maps a context to a
-- Boolean. The presence of a variable in a context means it is true. Absence
-- means it is false.
semExpr :: Expr -> Context -> Bool
semExpr (Var     x)     ctx = x `elem` ctx
semExpr (Not     e)     ctx = not $ semExpr e ctx
semExpr (And     e1 e2) ctx = semExpr e1 ctx && semExpr e2 ctx
semExpr (Or      e1 e2) ctx = semExpr e1 ctx || semExpr e2 ctx
semExpr (Implies e1 e2) ctx = semExpr (Or (Not e1) e2) ctx

-- The nand operation.
nand :: Bool -> Bool -> Bool
nand True True = False
nand _    _    = True

allSublists :: [a] -> [[a]]
allSublists xs = flip take xs <$> [0..length xs]

-- The list of all variables mentioned in a circuit.
inputs :: Circuit -> Context
inputs (Input x)     = [x]
inputs (Nand  c1 c2) = inputs c1 ++ inputs c2

-- Semantics of a circuit. Similar to `semExpr`.
semCirc :: Circuit -> Context -> Bool
semCirc (Input x)     ctx = x `elem` ctx
semCirc (Nand  c1 c2) ctx = nand (semCirc c1 ctx) (semCirc c2 ctx)

-- Tries to find a context refuting the equivalence of a circuit and an expression.
tryToRefuteEquivalence :: Circuit -> Expr -> [Context]
tryToRefuteEquivalence c e =
  catMaybes $ (\ctx -> if semCirc c ctx == semExpr e ctx then Nothing else Just ctx) <$> allSublists is
    where
      is = nub (inputs c ++ vars e)

explicitForm :: [Char] -> Context -> [(Char, Bool)]
explicitForm cs ctx = (\c -> (c, c `elem` ctx)) <$> cs

showCtx :: [Char] -> Context -> String
showCtx cs ctx =
  let
    mapsto s1 s2 = s1 ++ " |-> " ++ s2
    ic = intercalate ", "
  in
    unwords
      [ "{"
      , ic $ (\(c, b) -> show (Var c) `mapsto` show b) <$> explicitForm cs ctx
      , "}"
      ]

-- The following is useful for abstracting over a certain class of properties.

data BinConnective = Conj | Disj | Impl deriving (Eq)

showConn :: BinConnective -> String
showConn Conj = "And"
showConn Disj = "Or"
showConn Impl = "Implies"

vernConn :: BinConnective -> String
vernConn Conj = "conjunction"
vernConn Disj = "disjunction"
vernConn Impl = "implication"

binConnective :: BinConnective -> Expr -> Expr -> Expr
binConnective Conj = And
binConnective Disj = Or
binConnective Impl = Implies

sizedArbitraryExprOnlyNot :: Int -> Gen Expr
sizedArbitraryExprOnlyNot 0 = arbitraryVar
sizedArbitraryExprOnlyNot n = do t <- sizedArbitraryExprOnlyNot (n - 1)
                                 return (Not t)

arbitraryExprOnlyNot :: Gen Expr
arbitraryExprOnlyNot = do
  n <- chooseInt (0, 9)
  sizedArbitraryExprOnlyNot n

sizedArbitraryExprOnlyAnd :: Int -> Gen Expr
sizedArbitraryExprOnlyAnd 0 = arbitraryVar
sizedArbitraryExprOnlyAnd n =
  do e1 <- frequency [ (3, arbitraryVar)
                     , (2, sizedArbitraryExprOnlyAnd (n `div` 2))]
     e2 <- frequency [ (3, arbitraryVar)
                     , (2, sizedArbitraryExprOnlyAnd (n `div` 2))]
     return $ And e1 e2

arbitraryExprOnlyAnd :: Gen Expr
arbitraryExprOnlyAnd = do n <- chooseInt (0, 18)
                          sizedArbitraryExprOnlyAnd n

sizedArbitraryExprFreeOf :: Int -> BinConnective -> Gen Expr
sizedArbitraryExprFreeOf 0 = const arbitraryVar
sizedArbitraryExprFreeOf n = frequency . freqListFreeOf n

freqListFreeOf :: Int -> BinConnective -> [(Int, Gen Expr)]
freqListFreeOf n conn =
  let
    genNeg = do
      t <- sizedArbitraryExprFreeOf (n - 1) conn
      return (Not t)

    genCon :: (Expr -> Expr -> Expr) -> Gen Expr
    genCon c = do
      s <- sizedArbitraryExprFreeOf (n `div` 2) conn
      t <- sizedArbitraryExprFreeOf (n `div` 2) conn
      return (c s t)
  in
    case conn of
      Conj -> [ (3, arbitraryVar) ,  (2, genCon Or)  , (2, genCon Implies) , (2, genNeg) ]
      Disj -> [ (3, arbitraryVar) ,  (2, genCon And) , (2, genCon Implies) , (2, genNeg) ]
      Impl -> [ (3, arbitraryVar) ,  (2, genCon And) , (2, genCon Or)      , (2, genNeg) ]

arbitraryExprFreeOf :: BinConnective -> Gen Expr
arbitraryExprFreeOf conn = do
  n <- chooseInt (0, 18)
  sizedArbitraryExprFreeOf n conn

makeCircuitProperty :: Expr -> Property
makeCircuitProperty e =
  let
    stu' = Student.circuit e
    stu  = deepseq stu' stu'
    cs   = nub $ vars e ++ inputs stu
   in
     case tryToRefuteEquivalence stu e of
       []      -> property True
       (ref:_) -> let
                    msg  = unwords
                             [ "When your 'circuit' function was called on"
                             , singleQuotes $ show e
                             , "it returned a circuit"
                             , "that is not logically equivalent because"
                             , "under the assignment"
                             , indented (showCtx cs ref) ++ "it"
                             , "evaluates to"
                             , singleQuotes (show (semCirc stu ref))
                             , "whereas the expression " ++ singleQuotes (show e)
                             , "evaluates to "
                               ++ singleQuotes (show (semExpr e ref))
                             ]
                  in
                    counterexample msg False

-------------------------------------------------------------------------------
-- Tests: Question 5
-------------------------------------------------------------------------------

interleave :: [a] -> [a] -> [a]
interleave []       ys       = ys
interleave xs       []       = xs
interleave (x : xs) (y : ys) = x : y : interleave xs ys

instance (Eq a, Ord a, Arbitrary a) => Arbitrary (Heap a) where
  arbitrary = sized $ \n -> do
      k     <- chooseInt (0,n)
      elems <- replicateM k arbitrary
      go elems (length elems)
    where
      go :: (Eq a, Ord a) => [a] -> Int -> Gen (Heap a)
      go [] _ = pure Empty
      go xs n = do
          let x   = minimum xs
              xs' = delete x xs
          split <- chooseInt (0, n - 1)
          l     <- go (take split xs') split
          r     <- go (drop split xs') (n - 1 - split)
          pure $ HeapNode x l r

  -- the shrinkings of a MinHeap are all its subtrees
  -- we want to return shrinkings in order of size
  shrink Empty = []
  shrink (HeapNode _ l r) =
    interleave (shrink l) (shrink r) ++ [l , r]

instance NFData a => NFData (Heap a) where
 rnf Empty            = ()
 rnf (HeapNode x l r) = rnf x `seq` rnf l `seq` rnf r

getElems :: Heap a -> [a]
getElems Empty        = []
getElems (HeapNode x l r) = x : (getElems l ++ getElems r)

getAllPaths :: Heap a -> [[a]]
getAllPaths Empty                    = []
getAllPaths (HeapNode x Empty Empty) = [[x]]
getAllPaths (HeapNode x l r)         = map (x:) (getAllPaths l ++ getAllPaths r)

isIncreasing :: Ord a => [a] -> Bool
isIncreasing (x:y:path)
  | x <= y    = isIncreasing (y:path)
  | otherwise = False
isIncreasing _ = True

isMinHeap :: (Ord a, NFData a, Show a) => Heap a -> Property
isMinHeap heap =
  counterexample msg (isNothing falsifyingPath)
  where
    msg :: String
    msg = unwords
      [ "Your output heap is not a MinHeap as not all paths from the root are"
      , "decreasing. For example, consider the following path"
      , maybe "'oops you shouldn't see this'" show falsifyingPath, "."
      ]

    falsifyingPaths = filter (not . isIncreasing)
                    $ getAllPaths
                    $ deepseq heap heap

    falsifyingPath = listToMaybe falsifyingPaths

containsSameElems :: (Eq a, NFData a, Show a) => Heap a -> Heap a -> Property
containsSameElems heap1 heap2 =
  counterexample missingMsg (null missing) .&&.
  counterexample extrasMsg (null extras)
  where
    student  = getElems (deepseq heap1 heap1)
    solution = getElems (deepseq heap2 heap2)

    missing = solution \\ student
    extras  = student \\ solution

    missingMsg :: String
    missingMsg = unwords
      [ "Your output heap is missing the elements", show missing
      , "which it should contain."]
    extrasMsg :: String
    extrasMsg = unwords
      [ "Your output heap contains the elements", show extras
      , "which it should not."]

prop_insert_base :: Int -> Property
prop_insert_base n =
  Student.insert n Empty ~= HeapNode n Empty Empty

prop_insert_isMinHeap :: Int -> Heap Int -> Property
prop_insert_isMinHeap x heap = isMinHeap (Student.insert x heap)

prop_insert_insertsCorrectly :: Int -> Heap Int -> Property
prop_insert_insertsCorrectly x heap =
  containsSameElems (Student.insert x heap) (Solutions.insert x heap)

prop_popMin_getsMinVal :: Heap Int -> Property
prop_popMin_getsMinVal heap =
  fst (Student.popMin heap) ~= fst (Solutions.popMin heap)

prop_popMin_isMinHeap :: Heap Int -> Property
prop_popMin_isMinHeap heap = isMinHeap (snd $ Student.popMin heap)

prop_popMin_removesCorrectly :: Heap Int -> Property
prop_popMin_removesCorrectly heap =
  containsSameElems (snd $ Student.popMin heap) (snd $ Solutions.popMin heap)
