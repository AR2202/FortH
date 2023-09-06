{-# LANGUAGE OverloadedStrings #-}

import Data.IntMap as IM
import Data.List as L
import qualified Data.Map as Map
import Data.Text as T
import Eval
import ForthVal
import Parser
import Test.Hspec
import Test.QuickCheck
import Text.Parsec
-- import           Text.Parsec.Expr
import qualified Text.Parsec.Language as Lang
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Char

main :: IO ()
main =
  hspec $
    describe "initialEnv" $ do
      it "should start with an empty stack" $ property prop_initial_stack_empty
      describe "initialDef" $ do
        it "should contain +" initialDefs_addition_operator
        describe "ideToken" $
          it "should return an identifier Token" $
            parseIdExists
        describe "eval" $
          it "pushes result on top of the stack" $
            stackState (eval envWithStackNumbers (Arith Add))
              `shouldBe` Right [3, 4]
        describe "eval" $
          context "when evaluating a definition" $
            it
              "should insert the new word"
              newWordInserted
        describe "eval" $
          context "when evaluating multiplication" $
            it
              "pushes result on top of the stack"
              multiplyResultOnStack
        describe "eval" $
          context "when evaluating stack manipulation drop" $
            it
              "drops top of the stack"
              stackDrop
        describe "eval" $
          context "when evaluating stack manipulation dup" $
            it
              "duplicates top element of the stack"
              stackDup
        describe "eval" $
          context "when evaluating stack manipulation over" $
            it
              "repeats top of the stack as 3rd element"
              stackOver
        describe "eval" $
          context "when trying to use an undefined word" $
            it
              "throws UnknownWord error"
              unknownWordError
        describe "eval" $
          context "when defining a word and using it" $
            it
              "executes the new word"
              definedWord
        describe "eval" $
          context "when evaluating an execution token" $
            it
              "executes the definition at the address"
              addressExecuted
        describe "eval" $
          context "when evaluating a list of forthvals" $
            it
              "executes all of them"
              forthvalsAllExecuted
        describe "eval" $
          context "when evaluating if" $
            it
              "executes if stack top is not 0"
              ifExecutedIfTrue
        describe "eval" $
          context "when evaluating if" $
            it
              "does not execute if stack top is 0"
              ifNotExecutedIfFalse
        describe "eval" $
          context "when evaluating if else" $
            it
              "executes else if stack top is 0"
              elseExecutedIfFalse
        describe "eval" $
          context "when evaluating do loop" $
            it
              "executes from start to finish index"
              doloopExecutedntimes
        describe "eval" $
          context "when evaluating do loop" $
            it
              "loppbody is executed"
              doloopExecuted
        describe "eval" $
          context "when evaluating do loop with start and finish index equal" $
            it
              "loopbody is not executed"
              doloopnotExecuted
        describe "eval" $
          context "when using I in do loop " $
            it
              "puts the current index on the stack"
              indexPutOnStack
        describe "eval" $
          context "when evaluating plus loop" $
            it
              "loopbody is executed"
              ploopExecuted
        describe "eval" $
          context "when evaluating plus loop" $
            it
              "increases the index by number on top of stack"
              ploopIncreaseIndex
        describe "eval" $
          context "when evaluating until loop" $
            it
              "runs until stack top is True (non-zero)"
              untilloopRunUntilTrue
        describe "eval" $
          context "when evaluating until loop" $
            it
              "stops execution if stack top is 2"
              untilloopStopOn2
        describe "eval" $
          context "when acessing uninitialized variable" $
            it
              "throws exception"
              uninitializedVar
        describe "eval" $
          context "when initializing a variable and retrieving it" $
            it
              "puts memory address of new variable on stack"
              varExistsAfterInitialization
        describe "eval" $
          context "when assigning a variable and retrieving it" $
            it
              "puts value assigned to variable on stack"
              variableCanBeAssignedAndRetrieved
        describe "eval" $
          context "when evaluating memory storage and retrieval" $
            it
              "does the same as the ! and @ words"
              numCanBeStoredAndRetrieved
        describe "eval" $
          context "when evaluating cell allot" $
            it
              "allocates memory"
              cellAllotInitializesMemory
        

---------Test for initial environment----------
-----------------------------------------------
-- Unit tests----------

initialDefs_addition_operator :: Bool
initialDefs_addition_operator =
  case Map.lookup "+" (_names initialEnv) of
    Nothing -> False
    Just i ->
      case IM.lookup i (_definitions initialEnv) of
        Nothing -> False
        Just forthval -> forthval == Arith Add

-- property based tests----------

-- | test that initially the stack is empty
-- and binary operators have a stack underflow error
prop_initial_stack_empty :: Operator -> Bool
prop_initial_stack_empty op = eval initialEnv (Arith op) == Left StackUnderflow

-----------Tests for parsing----------
--------------------------------------
parseIdExists = parse ideToken "file" "aword" `shouldBe` Right (Ide "aword")

-----------Tests for evaluation-------
--------------------------------------
-- defining new words
newWordInserted :: Expectation
newWordInserted =
  eval initialEnv (Def (ForthVal.Fun "new" [Arith Add, Arith Times]))
    `shouldSatisfy` env_contains_word "new"

definedWord :: Expectation
definedWord =
  stackState
    ( envWithNewWord >>= (`eval` Word "new")
    )
    `shouldBe` Right [12]

unknownWordError :: Expectation
unknownWordError = eval initialEnv (Word "unkownWord") `shouldBe` Left UnknownWord

-- arithmetic
multiplyResultOnStack :: Expectation
multiplyResultOnStack =
  stackState (eval envWithStackNumbers (Arith Times))
    `shouldBe` Right [2, 4]

addResultOnStack :: Expectation
addResultOnStack =
  stackState (eval envWithStackNumbers (Arith Add))
    `shouldBe` Right [3, 4]

stackDrop :: Expectation
stackDrop =
  stackState (eval envWithStackNumbers (Manip Drop))
    `shouldBe` Right [2, 4]

-- Stack manipulation
stackDup :: Expectation
stackDup =
  stackState (eval envWithStackNumbers (Manip Dup))
    `shouldBe` Right [1, 1, 2, 4]

stackOver :: Expectation
stackOver =
  stackState (eval envWithStackNumbers (Manip Over))
    `shouldBe` Right [1, 2, 1, 4]

-- list of values
addressExecuted :: Expectation
addressExecuted =
  stackState (eval envWithStackNumbers (Address 0))
    `shouldBe` Right [3, 4]

forthvalsAllExecuted :: Expectation
forthvalsAllExecuted =
  stackState (eval envWithStackNumbers (Forthvals [Manip Drop, Manip Dup]))
    `shouldBe` Right [2, 2, 4]

-- if else
ifExecutedIfTrue :: Expectation
ifExecutedIfTrue =
  stackState (eval envWithStackNumbers (If [Manip Drop, Manip Dup]))
    `shouldBe` Right [4, 4]

ifNotExecutedIfFalse :: Expectation
ifNotExecutedIfFalse =
  stackState (eval envWithStackTop0 (If [Manip Drop, Manip Dup]))
    `shouldBe` Right [1, 2, 4]

elseExecutedIfFalse :: Expectation
elseExecutedIfFalse =
  stackState (eval envWithStackTop0 (IfElse [Manip Drop, Manip Dup] [Number 10]))
    `shouldBe` Right [10, 1, 2, 4]

-- loops
doloopExecuted :: Expectation
doloopExecuted =
  stackState (eval envWithStackTop0 (DoLoop Loop {_loopbody = [Number 3]}))
    `shouldBe` Right [3, 2, 4]

doloopExecutedntimes :: Expectation
doloopExecutedntimes =
  stackState (eval envWithStackTop0 (Forthvals [Number 5, Number 0, DoLoop Loop {_loopbody = [Number 3]}]))
    `shouldBe` Right [3, 3, 3, 3, 3, 0, 1, 2, 4]

doloopnotExecuted :: Expectation
doloopnotExecuted =
  stackState (eval envWithStackTop0 (Forthvals [Number 0, DoLoop Loop {_loopbody = [Number 3]}]))
    `shouldBe` Right [1, 2, 4]

indexPutOnStack :: Expectation
indexPutOnStack =
  stackState (eval initialEnv (Forthvals [Number 5, Number 0, DoLoop Loop {_loopbody = [Word "I"]}]))
    `shouldBe` Right [4, 3, 2, 1, 0]

ploopExecuted :: Expectation
ploopExecuted =
  stackState (eval envWithStackTop0 (PlusLoop Loop {_loopbody = [Number 1]}))
    `shouldBe` Right [1, 2, 4]

ploopIncreaseIndex :: Expectation
ploopIncreaseIndex =
  stackState (eval envWithStackNumbers (Forthvals [Number 5, Number 2, PlusLoop Loop {_loopbody = [Number 2]}]))
    `shouldBe` Right [2, 2, 1, 2, 4]

untilloopRunUntilTrue :: Expectation
untilloopRunUntilTrue =
  stackState (eval envWithStackNumbers (UntilLoop Loop {_loopbody = [Number 2, Arith Equal]}))
    `shouldBe` Right [4]

untilloopStopOn2 :: Expectation
untilloopStopOn2 =
  stackState (eval envWithStackNumbers (UntilLoop Loop {_loopbody = [Manip Drop]}))
    `shouldBe` Right [4]

-- Variables
uninitializedVar :: Expectation
uninitializedVar = eval initialEnv (Word "myvar") `shouldBe` Left UnknownWord

varExistsAfterInitialization :: Expectation
varExistsAfterInitialization = stackTail (eval initialEnv (Forthvals [Variable "myvar", Word "myvar"])) `shouldBe` Right []

variableCanBeAssignedAndRetrieved :: Expectation
variableCanBeAssignedAndRetrieved = stackTop (eval initialEnv (Forthvals [Variable "myvar", Number 2, Word "myvar", Word "!", Word "myvar", Word "@"])) `shouldBe` Right 2

-- Memory operations
numCanBeStoredAndRetrieved :: Expectation
numCanBeStoredAndRetrieved = stackTop (eval initialEnv (Forthvals [Variable "myvar", Number 2, Word "myvar", Mem Store, Word "myvar", Mem Retrieve])) `shouldBe` Right 2

cellAllotInitializesMemory :: Expectation
cellAllotInitializesMemory = stackState ((eval envWithStackNumbers (Mem Allot)) >>= (`eval` (Forthvals [Number 2, Mem Retrieve]))) `shouldBe` Right [0, 4]

-----------Helper functions------------
---------------------------------------

env_contains_word :: T.Text -> Either ForthErr Env -> Bool
env_contains_word _ (Left _) = False
env_contains_word text (Right env) = Map.member text (_names env)

envWithStackNumbers :: Env
envWithStackNumbers = Env initialNames initialDefs [1, 2, 4] (IM.singleton 0 0) 1 []

envWithStackTop0 :: Env
envWithStackTop0 = Env initialNames initialDefs [0, 1, 2, 4] (IM.singleton 0 0) 1 []

stackState :: Either ForthErr Env -> Either ForthErr [Int]
stackState = fmap _stack

stackTop :: Either ForthErr Env -> Either ForthErr Int
stackTop = fmap L.head . stackState

stackTail :: Either ForthErr Env -> Either ForthErr [Int]
stackTail = fmap L.tail . stackState

envWithNewWord :: Either ForthErr Env
envWithNewWord = eval envWithStackNumbers (Def (ForthVal.Fun "new" [Arith Add, Arith Times]))
