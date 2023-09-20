module Transpiler
  ( transpileExpression,
    ExpressionStack,
    ExpressionTree (..),
    transpileAndOutput,
  )
where

import Control.Concurrent (yield)
import Eval
import ForthVal
import Parser
import Test.Hspec (xcontext)

data ExpressionTree = Lit Int | Addition ExpressionTree ExpressionTree | Subtract ExpressionTree ExpressionTree deriving (Show, Read, Eq)

type ExpressionStack = [ExpressionTree]

transpileExpression :: [ForthVal] -> Either ForthErr [ExpressionTree]
transpileExpression [] = Right []
transpileExpression (x : xs) = go [] (x : xs)
  where
    go exps [] = Right exps
    go exps (Number a : xs) = go (Lit a : exps) xs
    go [] _ = Left StackUnderflow
    go [x] _ = Left StackUnderflow
    -- this is not currently produced by the parser, but will have to be
    go (x : y : zs) (Arith Add : xs) = go (Addition x y : zs) xs
    go (x : y : zs) (Arith Sub : xs) = go (Subtract y x : zs) xs
    go _ _ = Left ParseErr

produceOutput :: ExpressionTree -> String
produceOutput (Lit x) = show x
produceOutput (Addition x y) = produceOutput x ++ " + " ++ produceOutput y
produceOutput (Subtract (Lit x) (Lit y)) = show x ++ " - " ++ show y
produceOutput (Subtract (Lit x) y) = show x ++ " - (" ++ produceOutput y ++ ")"
produceOutput (Subtract x (Lit y)) = "(" ++ produceOutput x ++ ") - " ++ show y
produceOutput (Subtract x y) = "(" ++ produceOutput x ++ ") - (" ++ produceOutput y ++ ")"

showOutput :: Show a => Either a [ExpressionTree] -> IO ()
showOutput (Left err) = print err
showOutput (Right []) = putStrLn ""
showOutput (Right (x : xs)) = putStrLn $ produceOutput x

transpileAndOutput :: [ForthVal] -> IO ()
transpileAndOutput = showOutput . transpileExpression
