module Transpiler
  ( transpileExpression,
    ExpressionStack,
    ExpressionTree (..),
    transpileAndOutput,
    parseTranspileOutputFromText,
    parseTranspileOutputFromFile,
    parseTranspileGenerateOutputFromText,
    transpileText2File,
  )
where

import Control.Concurrent (waitQSem, yield)
import Data.Functor.Contravariant (Predicate (Predicate))
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Text as Text
import Eval
import ForthVal
import Parser
import System.FilePath.Lens (filename)
import Test.Hspec (xcontext)

data ExpressionTree = Lit Int | Addition ExpressionTree ExpressionTree | Subtract ExpressionTree ExpressionTree | Multiply ExpressionTree ExpressionTree | IntDiv ExpressionTree ExpressionTree | Print ExpressionTree deriving (Show, Read, Eq)

type ExpressionStack = [ExpressionTree]

transpileExpression :: [ForthVal] -> Either ForthErr [ExpressionTree]
transpileExpression [] = Right []
transpileExpression (x : xs) = go [] (x : xs) []
  where
    go [] [] returnstack = Right (Prelude.reverse returnstack)
    go (x : exps) [] returnstack = Right $ Prelude.reverse (x : returnstack)
    go exps (Number a : xs) returnstack = go (Lit a : exps) xs returnstack
    go [] _ _ = Left StackUnderflow
    go (x : xs) (PrintCommand : zs) returnstack = go xs zs (Print x : returnstack)
    go [x] _ _ = Left StackUnderflow
    -- this is not currently produced by the parser, but will have to be
    go (x : y : zs) (Arith Add : xs) returnstack = go (Addition y x : zs) xs returnstack
    go (x : y : zs) (Arith Sub : xs) returnstack = go (Subtract y x : zs) xs returnstack
    go (x : y : zs) (Arith Times : xs) returnstack = go (Multiply y x : zs) xs returnstack
    go (x : y : zs) (Arith Div : xs) returnstack = go (IntDiv y x : zs) xs returnstack
    go _ _ _ = Left ParseErr

produceOutput :: ExpressionTree -> String
produceOutput (Lit x) = show x
produceOutput (Addition x y) = produceOutput x ++ " + " ++ produceOutput y
produceOutput (Subtract (Lit x) (Lit y)) = show x ++ " - " ++ show y
produceOutput (Subtract (Lit x) y) = show x ++ " - (" ++ produceOutput y ++ ")"
produceOutput (Subtract x (Lit y)) = "(" ++ produceOutput x ++ ") - " ++ show y
produceOutput (Subtract x y) = "(" ++ produceOutput x ++ ") - (" ++ produceOutput y ++ ")"
produceOutput (Multiply (Lit x) (Lit y)) = show x ++ " * " ++ show y
produceOutput (Multiply (Lit x) y) = show x ++ " * (" ++ produceOutput y ++ ")"
produceOutput (Multiply x (Lit y)) = "(" ++ produceOutput x ++ ") * " ++ show y
produceOutput (Multiply x y) = "(" ++ produceOutput x ++ ") * (" ++ produceOutput y ++ ")"
produceOutput (IntDiv (Lit x) (Lit y)) = show x ++ " // " ++ show y
produceOutput (IntDiv (Lit x) y) = show x ++ " // (" ++ produceOutput y ++ ")"
produceOutput (IntDiv x (Lit y)) = "(" ++ produceOutput x ++ ") // " ++ show y
produceOutput (IntDiv x y) = "(" ++ produceOutput x ++ ") // (" ++ produceOutput y ++ ")"
produceOutput (Print x) = "print(" ++ produceOutput x ++ ")"

generateOutputString :: Either ForthErr ExpressionStack -> String
generateOutputString (Left err) = show err
generateOutputString (Right []) = ""
generateOutputString (Right (x : xs)) = Prelude.unlines $ Prelude.map produceOutput (x : xs)

showOutput :: Either ForthErr [ExpressionTree] -> IO ()
showOutput = putStrLn . generateOutputString

transpileAndOutput :: [ForthVal] -> IO ()
transpileAndOutput = showOutput . transpileExpression

lookupDefs :: ForthVal -> Either ForthErr ForthVal
lookupDefs (Word w) = lookupWord w
  where
    lookupWord w = case M.lookup w initialNames of
      Nothing -> Left UnknownWord
      Just i -> case IM.lookup i initialDefs of
        Nothing -> Left UnknownWord
        Just forthval -> Right forthval
lookupDefs f = Right f

lookupTranspile :: [ForthVal] -> Either ForthErr [ExpressionTree]
lookupTranspile vals = traverse lookupDefs vals >>= transpileExpression

lookupTranspileOutput :: [ForthVal] -> IO ()
lookupTranspileOutput = showOutput . lookupTranspile

parseExpressionTranspileFromText :: String -> Text.Text -> Either ForthErr [ExpressionTree]
parseExpressionTranspileFromText t s = parseFromText t s >>= lookupTranspile

parseTranspileOutput :: String -> Text -> IO ()
parseTranspileOutput t = showOutput . parseExpressionTranspileFromText t

parseTranspileGenerateOutput :: String -> Text -> String
parseTranspileGenerateOutput t = generateOutputString . parseExpressionTranspileFromText t

parseTranspileOutputFromText :: Text -> IO ()
parseTranspileOutputFromText = parseTranspileOutput "text"

parseTranspileGenerateOutputFromText :: Text -> String
parseTranspileGenerateOutputFromText = parseTranspileGenerateOutput "text"

parseTranspileOutputFromFile :: FilePath -> IO ()
parseTranspileOutputFromFile filename = do
  inputfile <- readFile filename
  parseTranspileOutput filename (Text.pack inputfile)

transpileText2File :: FilePath -> Text -> IO ()
transpileText2File outfile input = do
  case parseExpressionTranspileFromText "text" input of
    Left err -> print err
    Right [] -> writeFile outfile ""
    Right (x : xs) -> writeFile outfile $ Prelude.unlines $ Prelude.map produceOutput (x : xs)
