module Transpiler
  ( transpileExpression,
    ExpressionStack,
    ExpressionTree (..),
    transpileAndOutput,
    parseTranspileOutputFromText,
    parseTranspileOutputFromFile,
    parseTranspileGenerateOutputFromText
  )
where

import Control.Concurrent (waitQSem, yield)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Text as Text
import Eval
import ForthVal
import Parser
import System.FilePath.Lens (filename)
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

generateOutputString :: Either ForthErr ExpressionStack -> String
generateOutputString (Left err) = show err
generateOutputString (Right []) = ""
generateOutputString (Right (x : xs)) = produceOutput x

showOutput :: Show a => Either a [ExpressionTree] -> IO ()
showOutput (Left err) = print err
showOutput (Right []) = putStrLn ""
showOutput (Right (x : xs)) = putStrLn $ produceOutput x

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
