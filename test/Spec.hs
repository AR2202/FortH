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
newWordInserted =
  eval initialEnv (Def (ForthVal.Fun "new" [Arith Add, Arith Times]))
    `shouldSatisfy` env_contains_word "new"

multiplyResultOnStack =
  stackState (eval envWithStackNumbers (Arith Times))
    `shouldBe` Right [2, 4]

addResultOnStack =
  stackState (eval envWithStackNumbers (Arith Add))
    `shouldBe` Right [3, 4]

stackDrop =
  stackState (eval envWithStackNumbers (Manip Drop))
    `shouldBe` Right [2, 4]

stackDup =
  stackState (eval envWithStackNumbers (Manip Dup))
    `shouldBe` Right [1, 1, 2, 4]

stackOver =
  stackState (eval envWithStackNumbers (Manip Over))
    `shouldBe` Right [1, 2, 1, 4]

unknownWordError = eval initialEnv (Word "unkownWord") `shouldBe` Left UnknownWord

definedWord =
  stackState
    ( envWithNewWord >>= (`eval` Word "new")
    )
    `shouldBe` Right [12]

addressExecuted =
  stackState (eval envWithStackNumbers (Address 0))
    `shouldBe` Right [3, 4]

forthvalsAllExecuted =
  stackState (eval envWithStackNumbers (Forthvals [Manip Drop, Manip Dup]))
    `shouldBe` Right [2, 2, 4]

ifExecutedIfTrue =
  stackState (eval envWithStackNumbers (If [Manip Drop, Manip Dup]))
    `shouldBe` Right [4, 4]

ifNotExecutedIfFalse =
  stackState (eval envWithStackTop0 (If [Manip Drop, Manip Dup]))
    `shouldBe` Right [1, 2, 4]

elseExecutedIfFalse =
  stackState (eval envWithStackTop0 (IfElse [Manip Drop, Manip Dup] [Number 10]))
    `shouldBe` Right [10, 1, 2, 4]

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

stackTop = fmap L.head . stackState

envWithNewWord = eval envWithStackNumbers (Def (ForthVal.Fun "new" [Arith Add, Arith Times]))
