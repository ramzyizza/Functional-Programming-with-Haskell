module Test2Marking where

import Data.List       (unwords)
import Data.Functor    ((<&>))
import MarkingCore
import Test.QuickCheck (Arbitrary(shrink), chooseInt)
import Test2TestCases
import TestCasesUtils  (Marks(Marks, getMarks))
import Types

import qualified Test2          as Student
import qualified Test2Solutions as Solutions

main :: IO ()
main = runMarking tests True
  where
    tests = [
              -- Question 1                       -- Total: 12 marks
              test_pentaFast_base0                -- 1 mark
            , test_pentaFast_base1                -- 1 mark
            , test_pentaFast_base2                -- 1 mark
            , test_pentaFast_base3                -- 1 mark
            , test_pentaFast_base4                -- 1 mark
            , test_pentaFast_recursive            -- 7 marks

            -- Question 2                         -- Total: 12 marks
            , test_depositInvalidNoUpdate         -- 1 mark
            , test_depositInvalidFailLog          -- 2 marks
            , test_depositInvalidFailLogModComm   -- -> 1 mark
            , test_depositNonZeroUpdate           -- 1 mark
            , test_depositNonZeroLog              -- 2 marks
            , test_depositNonZeroLogModComm       -- -> 1 mark
            , test_withdrawInvalidNoUpdate        -- 1 mark
            , test_withdrawInvalidFailLog         -- 2 marks
            , test_withdrawInvalidFailLogModComm  -- -> 1 mark
            , test_withdrawValidUpdate            -- 1 marks
            , test_withdrawValidLog               -- 2 marks
            , test_withdrawValidLogModComm        -- -> 1 mark

            -- Question 3                         -- Total: 12 marks
            , test_runAll_baseCase                -- 2 marks
            , test_runAll_recursiveCase           -- 5 marks
            , test_runAll_correctOrder            -- 5 marks

            -- Question 4                         -- Total: 12 marks
            , test_circuitEasyNot                 -- 1 mark
            , test_circuitEasyAnd                 -- 1 mark
            , test_circuitEasyOr                  -- 1 mark
            , test_circuitEasyImplies             -- 1 mark
            , test_circuitHardOnlyNot             -- 2 marks
            , test_circuitHardOnlyAnd             -- 2 marks
            , test_circuitHardOrFree              -- 2 marks
            , test_circuitHardImpliesFree         -- 2 marks

            -- Question 5                         -- Total: 12 marks
            , test_insert_isMinHeap               -- 3 marks
            , test_insert_insertsCorrectly        -- 3 marks
            , test_popMin_getsMinVal              -- 2 marks
            , test_popMin_isMinHeap               -- 2 marks
            , test_popMin_removesCorrectly        -- 2 marks
            ]

-------------------------------------------------------------------------------
-- Tests: Question 1
-------------------------------------------------------------------------------

test_pentaFast_base0 = Test
  { mark        = getMarks m
  , description = newSection
                  ++
                  "Checking that 'pentaFast' works correctly for the input '0'..."
  , successMsg  = unwords
    [ "You got", show m, "because your 'pentaFast' function returned '0'"
    , "on input '0'."
    ]
  , failMsg     = "Your 'pentaFast' function did not work correctly for the input '0'."
  , prop        = makeNullaryProp prop_pentaFast_base0
  , condition   = Always
  }
  where
    m = Marks 1

test_pentaFast_base1 = Test
  { mark        = getMarks m
  , description = "Checking that 'pentaFast' works correctly for the input '1'..."
  , successMsg  = unwords
    [ "You got", show m, "because your 'pentaFast' function returned '1'"
    , "on input '1'."
    ]
  , failMsg     = "Your 'pentaFast' function did not work correctly for the input '1'."
  , prop        = makeNullaryProp prop_pentaFast_base1
  , condition   = Always
  }
  where
    m = Marks 1

test_pentaFast_base2 = Test
  { mark        = getMarks m
  , description = "Checking that 'pentaFast' works correctly for the input '2'..."
  , successMsg  = unwords
    [ "You got", show m, "because your 'pentaFast' function returned '2'"
    , "on input '2'."
    ]
  , failMsg     = "Your 'pentaFast' function did not work correctly for the input '2'."
  , prop        = makeNullaryProp prop_pentaFast_base2
  , condition   = Always
  }
  where
    m = Marks 1

test_pentaFast_base3 = Test
  { mark        = getMarks m
  , description = "Checking that 'pentaFast' works correctly for the input '3'..."
  , successMsg  = unwords
    [ "You got", show m, "because your 'pentaFast' function returned '3'"
    , "on input '3'."
    ]
  , failMsg     = "Your 'pentaFast' function did not work correctly for the input '3'."
  , prop        = makeNullaryProp prop_pentaFast_base3
  , condition   = Always
  }
  where
    m = Marks 1

test_pentaFast_base4 = Test
  { mark        = getMarks m
  , description = "Checking that 'pentaFast' works correctly for the input '4'..."
  , successMsg  = unwords
    [ "You got", show m, "because your 'pentaFast' function returned '4'"
    , "on input '4'."
    ]
  , failMsg     = "Your 'pentaFast' function did not work correctly for the input '4'."
  , prop        = makeNullaryProp prop_pentaFast_base4
  , condition   = Always
  }
  where
    m = Marks 1

test_pentaFast_recursive = Test
  { mark        = getMarks m
  , description = "Checking that 'pentaFast' performs the recursive step correctly..."
  , successMsg  = unwords
    [ "You got", show m, "because your 'pentaFast' function did performed the"
    , "recursive step correctly."
    ]
  , failMsg     = "Your 'pentaFast' function did not work perform the recursive step correctly."
  , prop        = makeUnaryPropWith
                    prop_pentaFast_recursive
                    arbitraryLarge
                    (filter (>= 5) . shrink)
  , condition   = Always
  }
  where
    m = Marks 7

-------------------------------------------------------------------------------
-- Tests: Question 2
-------------------------------------------------------------------------------

-- Checked.
test_depositInvalidNoUpdate :: Test
test_depositInvalidNoUpdate = Test
  { mark        = getMarks m
  , description = newSection
                  ++
                  unwords
                    [ "Checking that 'deposit' does not modify the balance"
                    , "when called with a non-positive amount..."
                    ]
  , successMsg  = unwords
                    [ "You got"
                    , show m
                    , "because your 'deposit' function avoided modifying the"
                    , "balance when called with a non-positive amount."
                    ]
  , failMsg     = unwords
                    [ "Your 'deposit' function updated the balance"
                    , "when called with a non-positive amount."
                    ]
  , prop        = makeBinaryPropWith
                    prop_depositInvalidNoBalanceUpdate
                    arbitraryNonPositiveAmountAndBankAccount
                    shrinkAmountAndBankAccount
  , condition   = Always
  }
  where
    m = Marks 1

-- Checked.
test_depositInvalidFailLog :: Test
test_depositInvalidFailLog = Test
  { mark        = getMarks m
  , description = unwords
                    [ "Checking that 'deposit'"
                    , "updates the transaction log with a 'DepositFailed'"
                    , "in the correct chronological order"
                    , "when called with a non-positive amount..."
                    ]
  , successMsg  = unwords
                    [ "You got"
                    , show m
                    , "because your 'deposit' function correctly"
                    , "updated the transaction log with a 'DepositFailed'"
                    , "in the correct chronological order"
                    , "when called with a non-positive amount."
                    ]
  , failMsg     = unwords
                    [ "Your 'deposit' function did not update the transaction log"
                    , "with a 'DepositFailed' in the correct"
                    , "chronological order"
                    , "when called with a non-positive amount."
                    ]
  , prop        = makeBinaryPropWith
                    prop_depositInvalidFailLog
                    arbitraryNonPositiveAmountAndBankAccount
                    shrinkAmountAndBankAccount
  , condition   = Always
  }
  where
    m = Marks 2

-- Checked.
test_depositInvalidFailLogModComm :: Test
test_depositInvalidFailLogModComm = Test
  { mark        = getMarks m
  , description = unwords
                    [ "Checking that 'deposit'"
                    , "updates the transaction log with a 'DepositFailed'"
                    , "(ignoring the order)"
                    , "when called with a non-positive amount..."
                    ]
  , successMsg  = unwords
                    [ "You got"
                    , show m
                    , "because your 'deposit' function"
                    , "updated the transaction log with a 'DepositFailed'"
                    , "(ignoring the order)"
                    , "when called with a non-positive amount."
                    ]
  , failMsg     = unwords
                    [ "Your 'deposit' function did not update the"
                    , "transaction log"
                    , "with a 'DepositFailed'"
                    , "(ignoring the order)"
                    , "when called with a non-positive amount."
                    ]
  , prop        = makeBinaryPropWith
                    prop_depositInvalidFailLogModComm
                    arbitraryNonPositiveAmountAndBankAccount
                    shrinkAmountAndBankAccount
  , condition   = IfFail test_depositInvalidFailLog
  }
  where
    m = Marks 1

test_depositNonZeroUpdate :: Test
test_depositNonZeroUpdate = Test
  { mark        = getMarks m
  , description = unwords
                    [ "Checking that 'deposit'"
                    , "correctly updates the balance"
                    , "when called with a positive deposit amount..."
                    ]
  , successMsg  = unwords
                    [ "You got"
                    , show m
                    , "because your 'deposit' function correctly"
                    , "updated the balance"
                    , "when called with a positive deposit amount."
                    ]
  , failMsg     = unwords
                    [ "Your 'deposit' function did not update the balance"
                    , "correctly when called with a positive deposit amount."
                    ]
  , prop        = makeBinaryPropWith
                    prop_depositNonZeroUpdate
                    arbitraryNonZeroAmountAndBankAccount
                    shrinkAmountAndBankAccount
  , condition   = Always
  }
  where
    m = Marks 1

test_depositNonZeroLog :: Test
test_depositNonZeroLog = Test
  { mark        = getMarks m
  , description = unwords
                    [ "Checking that 'deposit'"
                    , "updates the transaction log"
                    , "with a 'Deposit'"
                    , "in the correct chronological order"
                    , "when called with a positive amount..."
                    ]
  , successMsg  = unwords
                    [ "You got"
                    , show m
                    , "because your 'deposit' function correctly"
                    , "updated the transaction log"
                    , "with a 'Deposit'"
                    , "in the correct chronological order"
                    , "when called with a positive amount."
                    ]
  , failMsg     = unwords
                    [ "Your 'deposit' function did not update the"
                    , "transaction log"
                    , "with a 'Deposit'"
                    , "in the correct chronological order"
                    , "when called with a positive amount."
                    ]
  , prop        = makeBinaryPropWith
                    prop_depositNonZeroLog
                    arbitraryNonZeroAmountAndBankAccount
                    shrinkAmountAndBankAccount
  , condition   = Always
  }
  where
    m = Marks 2

test_depositNonZeroLogModComm :: Test
test_depositNonZeroLogModComm = Test
  { mark        = getMarks m
  , description = unwords
                    [ "Checking that 'deposit'"
                    , "updates the transaction log"
                    , "with a 'Deposit'"
                    , "(ignoring the order)"
                    , "when called with a positive amount..."
                    ]
  , successMsg  = unwords
                    [ "You got"
                    , show m
                    , "because your 'deposit' function correctly"
                    , "updated the transaction log"
                    , "with a 'Deposit'"
                    , "(ignoring the order)"
                    , "when called with a positive amount."
                    ]
  , failMsg     = unwords
                    [ "Your 'deposit' function did not update the"
                    , "transaction log"
                    , "with a 'Deposit'"
                    , "(ignoring the order)"
                    , "when called with a positive amount."
                    ]
  , prop        = makeBinaryPropWith
                    prop_depositNonZeroLogModComm
                    arbitraryNonZeroAmountAndBankAccount
                    shrinkAmountAndBankAccount
  , condition   = IfFail test_depositNonZeroLog
  }
  where
    m = Marks 1

test_withdrawInvalidNoUpdate :: Test
test_withdrawInvalidNoUpdate = Test
  { mark        = getMarks m
  , description = unwords
                    [ "Checking that 'withdraw'"
                    , "does not modify the balance"
                    , "when called with an amount greater than the balance..."
                    ]
  , successMsg  = unwords
                    [ "You got"
                    , show m
                    , "because your 'withdraw' function avoided"
                    , "modifying the balance when called with"
                    , "an amount greater than the balance."
                    ]
  , failMsg     = unwords
                    [ "Your 'withdraw' function updated the balance"
                    , "when called with an invalid amount."
                    ]
  , prop        = makeBinaryPropWith
                    prop_withdrawInvalidNoUpdate
                    arbitraryInvalidPair
                    shrinkAmountAndBankAccount
  , condition   = Always
  }
  where
    m = Marks 1

test_withdrawInvalidFailLog :: Test
test_withdrawInvalidFailLog = Test
  { mark        = getMarks m
  , description = unwords
                    [ "Checking that 'withdraw'"
                    , "updates the transaction log with a 'WithdrawalFailed'"
                    , "in the correct chronological order"
                    , "when called with an amount greater than the balance..."
                    ]
  , successMsg  = unwords
                    [ "You got"
                    , show m
                    , "because your 'withdraw' function correctly updated"
                    , "the transaction log with a 'WithdrawalFailed'"
                    , "in the correct chronological order"
                    , "when called with an amount greater than the balance."
                    ]
  , failMsg     = unwords
                    [ "Your 'withdraw' function did not update"
                    , "the transaction log with a"
                    , "'WithdrawalFailed' in the correct chronological order"
                    , "when called with an amount greater than the balance."
                    ]
  , prop        = makeBinaryPropWith
                    prop_withdrawInvalidFailLog
                    arbitraryInvalidPair
                    shrinkAmountAndBankAccount
  , condition   = Always
  }
  where
    m = Marks 2

test_withdrawInvalidFailLogModComm :: Test
test_withdrawInvalidFailLogModComm = Test
  { mark        = getMarks m
  , description = unwords
                    [ "Checking that 'withdraw'"
                    , "updates the transaction log with a 'WithdrawalFailed'"
                    , "(ignoring the order)"
                    , "when called with an amount"
                    , "greater than the balance..."
                    ]
  , successMsg  = unwords
                    [ "You got"
                    , show m
                    , "because your 'withdraw' function updated"
                    , "the transaction log with a 'WithdrawalFailed'"
                    , "(ignoring the order)"
                    , "when given"
                    , "an amount greater than the balance."
                    ]
  , failMsg     = unwords
                    [ "Your 'withdraw' function did not"
                    , "update the transaction log with a"
                    , "'WithdrawalFailed' (ignoring the order)"
                    , "when called with an amount greater than the balance."
                    ]
  , prop        = makeBinaryPropWith
                    prop_withdrawInvalidFailLogModComm
                    arbitraryInvalidPair
                    shrinkAmountAndBankAccount
  , condition   = IfFail test_withdrawInvalidFailLog
  }
  where
    m = Marks 1

test_withdrawValidUpdate :: Test
test_withdrawValidUpdate = Test
  { mark        = getMarks m
  , description = unwords
                    [ "Checking that 'withdraw'"
                    , "correctly updates the balance"
                    , "when called with a valid withdrawal amount..."
                    ]
  , successMsg  = unwords
                    [ "You got"
                    , show m
                    , "because your 'withdraw' function correctly"
                    , "updated the balance"
                    , "when called with a valid withdrawal amount."
                    ]
  , failMsg     = unwords
                    [ "Your 'withdraw' function did not"
                    , "update the balance correctly"
                    , "when called with a valid withdrawal amount."
                    ]
  , prop        = makeBinaryPropWith
                    prop_withdrawValidUpdate
                    arbitraryValidPair
                    shrinkAmountAndBankAccount
  , condition   = Always
  }
  where
    m = Marks 1

test_withdrawValidLog :: Test
test_withdrawValidLog = Test
  { mark        = getMarks m
  , description = unwords
                    [ "Checking that 'withdraw'"
                    , "updates the transaction log"
                    , "with a 'Withdrawal'"
                    , "in the correct chronological order"
                    , "when called with a valid amount..."
                    ]
  , successMsg  = unwords
                    [ "You got"
                    , show m
                    , "because your 'withdraw' function correctly updated"
                    , "the transaction log"
                    , "with a 'Withdrawal'"
                    , "in the correct chronological order"
                    , "when given a valid withdrawal amount."
                    ]
  , failMsg     = unwords
                    [ "Your 'withdraw' function did not"
                    , "update the transaction log"
                    , "with a 'Withdrawal'"
                    , "in the correct chronological order"
                    , "when called with a valid withdrawal amount."
                    ]
  , prop        = makeBinaryPropWith
                    prop_withdrawValidLog
                    arbitraryValidPair
                    shrinkAmountAndBankAccount
  , condition   = Always
  }
  where
    m = Marks 2

test_withdrawValidLogModComm :: Test
test_withdrawValidLogModComm = Test
  { mark        = getMarks m
  , description = unwords
                    [ "Checking that 'withdraw'"
                    , "updates the transaction"
                    , "log with a 'Withdrawal' (ignoring the order)"
                    , "when given a valid withdrawal amount..."
                    ]
  , successMsg  = unwords
                    [ "You got"
                    , show m
                    , "because your 'withdraw' function updated"
                    , "the transaction log with a 'Withdrawal'"
                    , "(ignoring the order)"
                    , "when given a valid withdrawal amount."
                    ]
  , failMsg     = unwords
                    [ "Your 'withdraw' function did not"
                    , "update the transaction log with a 'Withdrawal'"
                    , "(ignoring the order)"
                    , "when given a valid withdrawal amount."
                    ]
  , prop        = makeBinaryPropWith
                    prop_withdrawValidLogModComm
                    arbitraryValidPair
                    shrinkAmountAndBankAccount
  , condition   = IfFail test_withdrawValidLog
  }
  where
    m = Marks 1


-------------------------------------------------------------------------------
-- Tests: Question 3
-------------------------------------------------------------------------------

test_runAll_baseCase :: Test
test_runAll_baseCase = Test
  { mark        = getMarks m
  , description = newSection
                  ++
                  "Checking that 'runAll' works correctly for the base case..."
  , successMsg  = unwords
                    [ "You got"
                    , show m
                    , "because your 'runAll' function returned 'pure Leaf'"
                    , "for the empty tree."
                    ]
  , failMsg     = "Your 'runAll' function did not work correctly for the empty tree."
  , prop        = makeNullaryProp prop_runAll_baseCase
  , condition   = Always
  }
  where
    m = Marks 2

test_runAll_recursiveCase = Test
  { mark        = getMarks m
  , description = unwords
    [ "Checking that 'runAll' works correctly for the recursive step by"
    , "using the Identity monad..."
    ]
  , successMsg  =  unwords
    [ "You got", show m, "because your 'runAll' function built up the resulting"
    , "tree correctly, ignoring the order of traversal."
    ]
  , failMsg     = "Your 'runAll' function did not work execute the recursive step correctly."
  , prop        = makeUnaryPropWith
                    prop_runAll_recursiveCase
                    arbitraryRecCase
                    shrinkRecCase
  , condition   = Always
  }
  where
    m = Marks 5

test_runAll_correctOrder = Test
  { mark        = getMarks m
  , description = unwords
    [ "Checking that 'runAll' performs the correct traversal by"
    , "using the Writer monad..."
    ]
  , successMsg  =  unwords
    [ "You got", show m, "because your 'runAll' function traversed the tree"
    , "in an pre-order traversal."
    ]
  , failMsg     = "Your 'runAll' function did not traverse the tree correctly."
  , prop        = makeUnaryProp prop_runAll_correctOrder
  , condition   = Always
  }
  where
    m = Marks 5

-------------------------------------------------------------------------------
-- Tests: Question 4
-------------------------------------------------------------------------------

test_circuitEasyNot :: Test
test_circuitEasyNot = Test
  { mark        = getMarks m
  , description = newSection
                  ++
                  unwords
                    [ "Checking that your 'circuit' function"
                    , "works correctly on simple"
                    , "expressions of the form"
                    , "'Not (Var x)'..." ]
  , successMsg  = unwords
                    [ "You got"
                    , show m
                    , "because your 'circuit' function"
                    , "worked correctly"
                    , "on simple expressions of the form"
                    , singleQuotes "Not (Var x)" ++ "."
                    ]
  , failMsg     = unwords
                    [ "Your 'circuit' function"
                    , "did not work correctly on simple expressions of the form"
                    , singleQuotes "Not (Var x)" ++ "."
                    ]
  , prop        = makeUnaryPropWith
                    makeCircuitProperty
                    (arbitraryVar >>= return . Not)
                    shrink
  , condition   = Always
  }
  where
    m = Marks 1

makeBinConnectiveEasyCircuitTest :: BinConnective -> Test
makeBinConnectiveEasyCircuitTest conn = Test
  { mark        = getMarks m
  , description = unwords
                    [ "Checking that your 'circuit' function"
                    , "works correctly on simple"
                    , "expressions of the form"
                    , singleQuotes (showConn conn ++ " (Var x) (Var y)") ++ "..."
                    ]
  , successMsg  = unwords
                    [ "You got"
                    , show m
                    , "because your 'circuit' function"
                    , "worked correctly"
                    , "on simple expressions of the form"
                    , singleQuotes (showConn conn ++ " (Var x) (Var y)") ++ "."
                    ]
  , failMsg     = unwords
                    [ "Your 'circuit' function"
                    , "did not work correctly on simple expressions of the form"
                    , singleQuotes (showConn conn ++ " (Var x) (Var y)") ++ "."
                    ]
  , prop        = makeUnaryPropWith
                   makeCircuitProperty
                   (arbitraryVarPair <&> uncurry (binConnective conn))
                   shrink
  , condition   = Always
  }
  where
    m = Marks 1

test_circuitEasyAnd, test_circuitEasyOr, test_circuitEasyImplies :: Test

test_circuitEasyAnd     = makeBinConnectiveEasyCircuitTest Conj
test_circuitEasyOr      = makeBinConnectiveEasyCircuitTest Disj
test_circuitEasyImplies = makeBinConnectiveEasyCircuitTest Impl

test_circuitHardOnlyNot :: Test
test_circuitHardOnlyNot = Test
  { mark        = getMarks m
  , description = unwords
                    [ "Checking that your 'circuit' function"
                    , "works correctly on expressions containing only 'Not'"
                    , "as a connective..."
                    ]
  , successMsg  = unwords
                    [ "You got"
                    , show m
                    , "because your 'circuit' function"
                    , "worked correctly on expressions containing only 'Not'"
                    , "as a connective."
                    ]
  , failMsg    = unwords
                   [ "Your 'circuit' function did not work correctly"
                   , "on expressions containing only 'Not' as a connective."
                   ]
  , prop       = makeUnaryPropWith
                   makeCircuitProperty
                   arbitraryExprOnlyNot
                   shrink
  , condition  = Always
  }
  where
    m = Marks 2


test_circuitHardOnlyAnd :: Test
test_circuitHardOnlyAnd = Test
  { mark        = getMarks m
  , description = unwords
                  [ "Checking that your 'circuit' function"
                  , "works correctly on expressions containing only 'And'"
                  , "as a connective..."
                  ]
  , successMsg = unwords
                   [ "You got"
                   , show m
                   , "because your 'circuit' function"
                   , "worked correctly on expressions containing only 'And'"
                   , "as a connective."
                   ]
  , failMsg    = unwords
                   [ "Your 'circuit' function did not work correctly"
                   , "on expressions containing only 'And' as a connective."
                   ]
  , prop       = makeUnaryPropWith
                   makeCircuitProperty
                   arbitraryExprOnlyAnd
                   shrink
  , condition  = Always
  }
  where
    m = Marks 2

makeCircuitBinConnectiveHardTest :: BinConnective -> Test
makeCircuitBinConnectiveHardTest conn = Test
  { mark        = getMarks m
  , description = unwords
                    [ "Checking that your 'circuit' function"
                    , "works correctly on expressions free of"
                    , vernConn conn ++ "s..."
                    ]
  , successMsg  = unwords
                    [ "You got"
                    , show m
                    , "because your 'circuit' function"
                    , "worked correctly on expressions free of"
                    , vernConn conn ++ "s."
                    ]
  , failMsg     = unwords
                    [ "Your 'circuit' function"
                    , "did not work correctly on expressions free of"
                    , vernConn conn ++ "s."
                    ]
  , prop        = makeUnaryPropWith
                    makeCircuitProperty
                    (arbitraryExprFreeOf conn)
                    shrink
  , condition   = Always
  }
  where
    m = Marks 2

test_circuitHardOrFree      :: Test
test_circuitHardImpliesFree :: Test

test_circuitHardOrFree      = makeCircuitBinConnectiveHardTest Disj
test_circuitHardImpliesFree = makeCircuitBinConnectiveHardTest Impl

-------------------------------------------------------------------------------
-- Tests: Question 5
-------------------------------------------------------------------------------

test_insert_isMinHeap = Test
  { mark        = getMarks m
  , description = newSection
                  ++ "Checking that 'insert' preserves the MinHeap property..."
  , successMsg  =  unwords
    [ "You got", show m, "because your 'insert' function preserved the MinHeap"
    , "property."
    ]
  , failMsg     = "Your 'insert' function did not preserve the MinHeap property."
  , prop        = makeBinaryProp prop_insert_isMinHeap
  , condition   = Always
  }
  where
    m = Marks 3

test_insert_insertsCorrectly = Test
  { mark        = getMarks m
  , description = "Checking that 'insert' inserts elements correctly..."
  , successMsg  =  unwords
    [ "You got", show m, "because your 'insert' function inserted a new element"
    , "correctly."
    ]
  , failMsg     = "Your 'insert' function did not insert an element correctly."
  , prop        = makeBinaryProp prop_insert_insertsCorrectly
  , condition   = Always
  }
  where
    m = Marks 3

test_popMin_getsMinVal = Test
  { mark        = getMarks m
  , description = "Checking that 'popMin' returns the root..."
  , successMsg  =  unwords
    [ "You got", show m, "because your 'popMin' returns the root." ]
  , failMsg     = "Your 'popMin' function did not return the root."
  , prop        = makeUnaryProp prop_popMin_getsMinVal
  , condition   = Always
  }
  where
    m = Marks 2

test_popMin_isMinHeap = Test
  { mark        = getMarks m
  , description = "Checking that 'popMin' preserves the MinHeap property..."
  , successMsg  =  unwords
    [ "You got", show m, "because your 'popMin' function preserved the MinHeap"
    , "property."
    ]
  , failMsg     = "Your 'popMin' function did not preserve the MinHeap property."
  , prop        = makeUnaryProp prop_popMin_isMinHeap
  , condition   = Always
  }
  where
    m = Marks 2

test_popMin_removesCorrectly = Test
  { mark        = getMarks m
  , description = "Checking that 'popMin' removes the root correctly..."
  , successMsg  =  unwords
    [ "You got", show m, "because your 'popMin' function removed the root"
    , "correctly."
    ]
  , failMsg     = "Your 'popMin' function did not remove the root correctly."
  , prop        = makeUnaryProp prop_popMin_removesCorrectly
  , condition   = Always
  }
  where
    m = Marks 2
