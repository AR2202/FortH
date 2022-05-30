{-# LANGUAGE OverloadedStrings #-}

import           Data.IntMap     as IM
import           Data.List       as L
import qualified Data.Map        as Map
import           Data.Text       as T
import           Eval
import           ForthVal
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main =
  hspec $
  describe "initialEnv" $ do
    it "should start with an empty stack" $ property prop_initial_stack_empty
    describe "initialDef" $ do
      it "should contain +" $ initialDefs_addition_operator `shouldBe` True
      describe "eval" $
        context "when evaluating a definition" $
        it "should insert the new word" $
        eval initialEnv (Def (ForthVal.Fun "new" "+ *")) `shouldSatisfy`
        env_contains_word "new"

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
