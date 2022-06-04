{-# LANGUAGE OverloadedStrings #-}

import           Data.IntMap                        as IM
import           Data.List                          as L
import qualified Data.Map                           as Map
import           Data.Text                          as T
import           Eval
import           ForthVal
import           Parser
import           Test.Hspec
import           Test.QuickCheck
import           Text.Parsec

--import           Text.Parsec.Expr
import qualified Text.Parsec.Language               as Lang
import           Text.Parsec.Text
import qualified Text.Parsec.Token                  as Tok
import           Text.ParserCombinators.Parsec.Char

main :: IO ()
main =
  hspec $
  describe "initialEnv" $ do
    it "should start with an empty stack" $ property prop_initial_stack_empty
    describe "initialDef" $ do
      it "should contain +" $ initialDefs_addition_operator `shouldBe` True
      describe "ideToken" $
        it "should return an identifier Token" $
        parse ideToken "file" "aword" `shouldBe` Right (Ide "aword")
      describe "eval" $
        it "pushes result on top of the stack" $
        stackState (eval envWithStackNumbers (Arith Add)) `shouldBe`
        Right [3, 4]
      describe "eval" $
        context "when evaluating a definition" $
        it "should insert the new word" $
        eval initialEnv (Def (ForthVal.Fun "new" "+ *")) `shouldSatisfy`
        env_contains_word "new"
      describe "eval" $
        context "when evaluating multiplication" $
        it "pushes result on top of the stack" $
        stackState (eval envWithStackNumbers (Arith Times)) `shouldBe`
        Right [2, 4]

initialDefs_addition_operator :: Bool
initialDefs_addition_operator =
  case Map.lookup "+" (names initialEnv) of
    Nothing -> False
    Just i ->
      case IM.lookup i (definitions initialEnv) of
        Nothing       -> False
        Just forthval -> forthval == Arith Add

prop_initial_stack_empty :: Operator -> Bool
prop_initial_stack_empty op = eval initialEnv (Arith op) == Left StackUnderflow

env_contains_word :: T.Text -> Either ForthErr Env -> Bool
env_contains_word _ (Left _)       = False
env_contains_word text (Right env) = Map.member text (names env)

envWithStackNumbers :: Env
envWithStackNumbers = Env initialNames initialDefs [1, 2, 4]

stackState :: Either ForthErr Env -> Either ForthErr [Int]
stackState = fmap stack
