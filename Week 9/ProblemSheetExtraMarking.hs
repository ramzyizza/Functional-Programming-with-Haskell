module ProblemSheetExtraMarking where

import MarkingCore
import ProblemSheetExtraTestCases

import Test.QuickCheck

main :: IO ()
main = runMarking tests True
  where
    tests = [ test_choose_IO
            , test_choose_List
            , test_choose_Dist
            , test_simulate_IO_True
            , test_simulate_IO_False
            , test_simulate_List
            , test_simulate_Dist
            , test_cut_io
            , test_cut_list_sub
            , test_cut_list_sup
            , test_cut_dist
            , test_shuffle_io
            , test_shuffle_list_sub
            , test_shuffle_list_sup
            , test_shuffle_dist
            , test_riffles_io
            , test_riffles_list
            , test_riffles_dist
            , test_riffles_num_calls_cut
            , test_riffles_num_calls_shuffle
            , test_permute_io
            , test_permute_list
            , test_permute_list_sup
            , test_permute_dist
            , test_genTree_io
            , test_genTree_list
            , test_genTree_list_sup
            , test_genTree_dist
            ]

{- Exercise 1 -}

-- choose
test_choose_IO = Test
  { mark        = n
  , description = newSection ++
                  "Checking that 'choose' in the IO monad correctly picks an \
                   \element from the given list..."
  , successMsg = "You got " ++ show n ++ " marks because 'choose' in the IO \
                  \monad correcty picks an element from the given list."
  , failMsg     = "'choose' did not work correctly in the IO monad."
  , prop        = makeUnaryProp prop_choose_IO
  , condition   = Always
  }
  where
    n = 2

test_choose_List = Test
  { mark        = n
  , description = "Checking that 'choose' in the List monad works correctly..."
  , successMsg = "You got " ++ show n ++ " marks because 'choose' in the list \
                  \ monad works correcty."
  , failMsg     = "'choose' did not work correctly in the List monad."
  , prop        = makeUnaryProp prop_choose_List
  , condition   = Always
  }
  where
    n = 2

test_choose_Dist = Test
  { mark        = n
  , description = "Checking that 'choose' in the Dist monad works correctly..."
  , successMsg = "You got " ++ show n ++ " marks because 'choose' in the Dist \
                  \monad works correcty."
  , failMsg     = "'choose' did not work correctly in the Dist monad."
  , prop        = makeUnaryProp prop_choose_Dist
  , condition   = Always
  }
  where
    n = 8

-- simulate
test_simulate_IO_True = Test
  { mark        = n
  , description = newSection ++
                  "Checking that 'simulate' in the IO monad works correctly on \
                   \experiments that always return 'True'..."
  , successMsg = "You got " ++ show n ++ " marks because 'simulate' in the IO \
                  \monad works correcty on experiments that always return 'True'."
  , failMsg     = "'simulate' did not work correctly in the IO monad."
  , prop        = makeUnaryProp prop_simulate_IO_True
  , condition   = Always
  }
  where
    n = 1

test_simulate_IO_False = Test
  { mark        = n
  , description = "Checking that 'simulate' in the IO monad works correctly on \
                   \experiments that always return 'False'..."
  , successMsg = "You got " ++ show n ++ " marks because 'simulate' in the IO \
                  \monad works correcty on experiments that always return 'False'."
  , failMsg     = "'simulate' did not work correctly in the IO monad."
  , prop        = makeUnaryProp prop_simulate_IO_False
  , condition   = Always
  }
  where
    n = 1


arbitrarySimulateList :: Gen ([Bool],Integer)
arbitrarySimulateList = do
  bs <- arbitrary `suchThat` (\xs -> length xs > 0 && length xs <= 4)
  i  <- arbitrary `suchThat` (\x -> x >= 0 && x <= 8)
  return (bs,i)

{-
   Note that we duplicate the above restrictions in prop_simulate_List and
   prop_simulate_Dist, because we need to check again in view of shrinking.
-}

test_simulate_List = Test
  { mark        = n
  , description = "Checking that 'simulate' in the List monad works correctly \
                   \on a list of booleans..."
  , successMsg = "You got " ++ show n ++ " marks because 'simulate' in the List \
                  \monad works correcty."
  , failMsg     = "'simulate' did not work correctly in the List monad."
  , prop        = makeBinaryPropWith' prop_simulate_List 2000000 -- increase timeout to 2s
                   arbitrarySimulateList shrink
  , condition   = Always
  }
  where
    n = 4

test_simulate_Dist = Test
  { mark        = n
  , description = "Checking that 'simulate' in the Dist monad works correctly \
                   \on distributions generated by\
                   \\n\t'normalise $ Solutions.choose bs'\
                   \\nwhere 'bs' is a list of booleans..."
  , successMsg = "You got " ++ show n ++ " marks because 'simulate' in the Dist \
                  \monad works correcty."
  , failMsg     = "'simulate' did not work correctly in the Dist monad."
  , prop        = makeBinaryPropWith' prop_simulate_Dist 2000000
                   arbitrarySimulateList shrink
  , condition   = Always
  }
  where
    n = 7

{- Question 2 -}

-- cut
test_cut_io = Test {
  mark = 2,
  description =  newSection ++ "Checking your 'cut' with the IO monad...",
  successMsg = "You got 2 marks as your 'cut' function works with the IO monad.",
  failMsg = "The output of 'cut' should concatenate to give the input to cut.",
  prop = makeUnaryProp prop_cut_io,
  condition = Always
}

test_cut_list_sub = Test {
  mark = 1,
  description = "Checking your 'cut' is correct for the List monad (1/2) by checking all returned cuts are valid...",
  successMsg = "You got 1 mark for having 'cut' return only cuts that are valid with the List monad.",
  failMsg = "Your 'cut' returns items not in the correct solution with the List monad.",
  prop = makeUnaryProp prop_cut_list_sub,
  condition = Always
}

test_cut_list_sup = Test {
  mark = 1,
  description = "Checking your 'cut' is correct for the List monad (2/2) by checking all valid cuts are returned...",
  successMsg = "You got 1 mark for having 'cut' return all valid cuts with the List monad.",
  failMsg = "The output of your 'cut' does not contain all valid cuts.",
  prop = makeUnaryProp prop_cut_list_sup,
  condition = Always
}

test_cut_dist = Test {
  mark = 4,
  description = "Checking your 'cut' with the Dist monad",
  successMsg = "You got 4 marks as your 'cut' functions correctly with the Dist monad.",
  failMsg = "Your 'cut' did not return the correct distibution with the Dist monad.",
  prop = makeUnaryProp prop_cut_dist,
  condition = Always
}

-- shuffle
test_shuffle_io = Test {
  mark = 2,
  description =  newSection ++ "Checking your 'shuffle' with the IO monad...",
  successMsg = "You got 2 marks as 'shuffle' with IO monad returns a valid interleaving.",
  failMsg = "Your 'shuffle' does not return a valid interleaving with the IO monad.",
  prop = makeUnaryProp prop_shuffle_io,
  condition = Always
}

test_shuffle_list_sub = Test {
  mark = 1,
  description = "Checking your 'shuffle' with the List monad (1/2) by checking all returned lists are valid interleavings...",
  successMsg = "You got 1 mark for having 'shuffle' only return valid interleavings with the List monad.",
  failMsg = "Your 'shuffle' returns lists which are not valid interleavings when ran with the List monad.",
  prop = makeUnaryPropWith prop_shuffle_list_sub (suchThat arbitrary shuffleSizeCheck) shrink,
  condition = Always
}

test_shuffle_list_sup = Test {
  mark = 1,
  description = "Checking your 'shuffle' with the List monad (2/2) by checking all valid interleavings are returned...",
  successMsg = "You got 1 mark for having 'shuffle' return all valid interleavings with the List monad.",
  failMsg = "Your 'shuffle' does not return all valid interleavings When ran with the List monad.",
  prop = makeUnaryPropWith prop_shuffle_list_sup (suchThat arbitrary shuffleSizeCheck) shrink,
  condition = Always
}

test_shuffle_dist = Test {
  mark = 4,
  description = "Checking your 'shuffle' with the Dist monad...",
  successMsg = "You got 4 marks as your 'shuffle' returns the correct distribution.",
  failMsg = "Your 'shuffle' does not return the correct distribution.",
  prop = makeUnaryPropWith prop_shuffle_dist (suchThat arbitrary shuffleSizeCheck) shrink,
  condition = Always
}

-- riffles
test_riffles_io = Test {
  mark = 1,
  description =  newSection ++ "Checking your 'riffles' with the IO monad...",
  successMsg = "You got 1 mark as your 'riffles' returns a permutation of its input list with the IO monad.",
  failMsg = "'riffles' should always return a permutation of its input list with the IO monad.",
  prop = makeBinaryPropWith prop_riffles_io (suchThat arbitrary rifflesSizeCheck) shrink,
  condition = Always
}

test_riffles_list = Test {
  mark = 1,
  description = "Checking your 'riffles' with the List monad by checking its length is correct...",
  successMsg = "You got 1 mark as your 'riffles' returns a list of the correct size.",
  failMsg = "Your 'riffles' does not return the correct distribution.",
  prop = makeBinaryPropWith prop_riffles_list (suchThat arbitrary rifflesSizeCheck) shrink,
  condition = Always
}

test_riffles_dist = Test {
  mark = 4,
  description = "Checking your 'riffles' with the Dist monad...",
  successMsg = "You got 4 marks as your 'riffles' returns the correct distribution.",
  failMsg = "Your 'riffles' does not return the correct distribution.",
  prop = makeBinaryPropWith' prop_riffles_dist 2000000 (suchThat arbitrary rifflesSizeCheck) shrink,
  condition = Always
}

test_riffles_num_calls_cut = Test {
  mark = 0,
  description = "Checking 'riffles' calls 'cut' and 'shuffle' the correct number of times...",
  successMsg = "",
  failMsg = "'riffles cf sf n xs' should call 'cf' 'n' times.",
  prop = makeBinaryPropWith prop_riffles_num_calls_cut (suchThat arbitrary rifflesSizeCheck) shrink,
  condition = Always
}

test_riffles_num_calls_shuffle = Test {
  mark = 3,
  description = "",
  successMsg = "You got 3 marks as your 'riffles' calls 'cut' and 'shuffle' the correct number of times.",
  failMsg = "'riffles cf sf n xs' should call 'sf' 'n' times.",
  prop = makeBinaryPropWith prop_riffles_num_calls_shuffle (suchThat arbitrary rifflesSizeCheck) shrink,
  condition = IfSuccess test_riffles_num_calls_cut
}

-------------------------------------------------------------------------------
-- Exercise 3  ----------------------------------------------------------------
-------------------------------------------------------------------------------

test_permute_io = Test {
  mark        = 10,
  description = newSection ++ "Checking 'permute' with the IO monad on the large input of\
                \ [1..1000]...",
  successMsg  = "You got 10 marks as your 'permute' worked correctly on the\
                \ large input [1..1000]",
  failMsg     = "'permute' did not work correctly on the large input of\
                \ [1..1000] (was either too slow or did not return a\
                \ permutation).",
  prop        = makeNullaryProp prop_permute_io,
  condition   = Always
}

test_permute_list = Test {
  mark        = 5,
  description = "Checking 'permute' with the List monad...",
  successMsg  = "You got 5 marks as your 'permute' worked correctly with the\
                \ List monad.",
  failMsg     = "Your implementation of 'permute' did not work correctly with\
                \ the List monad.",
  prop        = makeUnaryPropWith prop_permute_list
                  (arbitrarySmallList 6) shrink,
  condition   = Always
}

test_permute_list_sup = Test {
  mark        = 2,
  description = "Checking that 'permute' returns all permutations of its\
                \ input...",
  successMsg  = "You got 2 marks because your implementation of 'permute'\
                \ correctly returned all permutations of its input with the\
                \ List monad.",
  failMsg     = "'permute' should return all possible permutations with the\
                \ List monad.",
  prop        = makeUnaryPropWith prop_permute_list_sup
                  (arbitrarySmallList 6) shrink,
  condition   = IfFail test_permute_list
}

test_permute_dist = Test {
  mark        = 10,
  description = "Checking 'permute' with the Dist monad...",
  successMsg  = "You got 10 marks as your 'permute' worked correctly with the\
                \ Dist monad.",
  failMsg     = "Your 'permute' does not return the correct distribution.",
  prop        = makeUnaryPropWith prop_permute_dist
                  (arbitrarySmallList 6) shrink,
  condition   = Always
}

--------------------------------------------------------------------------------
-- Exercise 4  -----------------------------------------------------------------
--------------------------------------------------------------------------------

test_genTree_io = Test {
  mark        = 10,
  description = newSection ++ "Checking that your implementation of 'genTree' works correctly\
                \ with the IO monad on the large input of [1..150]...",
  successMsg  = "You got 10 marks as your 'genTree' computed a valid tree with\
                \ the IO monad on the large input [1..150].",
  failMsg     = "Your implementation of 'genTree' did not work correctly on the\
                \ large input [1..150].",
  prop        = makeNullaryProp prop_genTree_io,
  condition   = Always
}

test_genTree_list = Test {
  mark        = 5,
  description = "Checking 'genTree' with the List monad...",
  successMsg  = "You got 5 marks as your 'genTree' is correct with the List monad.",
  failMsg     = "Your 'genTree' did not work correctly with the List monad.\
                \ Checking for partial marks...",
  prop        = makeUnaryPropWith prop_genTree_list
                 (arbitrarySmallList 5) shrink,
  condition   = Always
}

test_genTree_list_sup = Test {
  mark        = 2,
  description = "Checking that your implementation of 'genTree' returns all\
                \ correct trees...",
  successMsg  = "You got 2 marks for having 'genTree' return all possible\
                \ correct trees with the List monad.",
  failMsg     = "'genTree' should return all correct trees with the List monad.",
  prop        = makeUnaryPropWith prop_genTree_list_sup
                 (arbitrarySmallList 5) shrink,
  condition   = IfFail test_genTree_list
}

test_genTree_dist = Test {
  mark        = 10,
  description = "Checking 'genTree' with the Dist monad",
  successMsg  = "You got 10 marks as 'genTree' was correct with the Dist monad.",
  failMsg     = "Your 'genTree' did not return the correct distribution.",
  prop        = makeUnaryPropWith prop_genTree_dist
                 (arbitrarySmallList 4) shrink,
  condition   = Always
}
