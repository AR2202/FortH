{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Transpiler
  ( transpileExpressionTree,
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

data ExpressionTree = Exp ArithExpression | Prt PrintExpression | Cond IfExpression | Loops LoopExpression | Varname String deriving (Read, Eq)

instance Show ExpressionTree where
  show (Exp x) = show x
  show (Prt x) = show x
  show (Cond x) = show x
  show (Varname x) = x
  show (Loops x) = show x

data ArithExpression = Lit Int | Addition ExpressionTree ExpressionTree | Subtract ExpressionTree ExpressionTree | Multiply ExpressionTree ExpressionTree | IntDiv ExpressionTree ExpressionTree | Equ ExpressionTree ExpressionTree | Lt ExpressionTree ExpressionTree | Gt ExpressionTree ExpressionTree | AND ExpressionTree ExpressionTree | OR ExpressionTree ExpressionTree | XOR ExpressionTree ExpressionTree | MOD ExpressionTree ExpressionTree | NOT ExpressionTree deriving (Read, Eq)

instance Show ArithExpression where
  show :: ArithExpression -> String
  show (Lit x) = show x
  show (Addition x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Subtract x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Multiply x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (IntDiv x y) = "(" ++ show x ++ " // " ++ show y ++ ")"
  show (Equ x y) = "(" ++ show x ++ " == " ++ show y ++ ")"
  show (Gt x y) = "(" ++ show x ++ " > " ++ show y ++ ")"
  show (Lt x y) = "(" ++ show x ++ " < " ++ show y ++ ")"
  show (AND x y) = "(bool(" ++ show x ++ ") and bool(" ++ show y ++ "))"
  show (OR x y) = "(bool(" ++ show x ++ ") or bool(" ++ show y ++ "))"
  show (XOR x y) = "(bool(" ++ show x ++ ") != bool(" ++ show y ++ "))"
  show (MOD x y) = "(" ++ show x ++ " % " ++ show y ++ ")"
  show (NOT x) = "not bool(" ++ show x ++ ")"

data PrintExpression = Print ArithExpression | PrintLit String | PrintVar String deriving (Read, Eq)

instance Show PrintExpression where
  show (Print (Lit x)) = "print(" ++ show x ++ ")"
  show (Print x) = "print" ++ show x
  show (PrintLit s) = "print(" ++ show s ++ ")"
  show (PrintVar s) = "print(" ++ s ++ ")"

data IfExpression = IfExp ExpressionTree ExpressionStack | IfElseExp ExpressionTree ExpressionStack ExpressionStack deriving (Read, Eq)

instance Show IfExpression where
  show (IfExp cond e) = "if " ++ produceOutput cond ++ ":\n" ++ Prelude.init(Prelude.unlines (Prelude.map ("    " ++) (Prelude.lines (Prelude.unlines (Prelude.map produceOutput e)))))
  show (IfElseExp cond i e) = "if " ++ produceOutput cond ++ ":\n" ++ Prelude.init(Prelude.unlines (Prelude.map ("    " ++) (Prelude.lines (Prelude.unlines (Prelude.map produceOutput i))))) ++ "\nelse:\n" ++ Prelude.init(Prelude.unlines (Prelude.map ("    " ++) (Prelude.lines (Prelude.unlines (Prelude.map produceOutput e)))))

data LoopExpression = Doloop ExpressionTree ExpressionTree ExpressionStack deriving (Read, Eq)

instance Show LoopExpression where
  show (Doloop start end exp) = "for i in range(" ++ produceOutput start ++ "," ++ produceOutput end ++ "):\n" ++ Prelude.unlines (Prelude.map ("    " ++) (Prelude.lines (Prelude.unlines (Prelude.map produceOutput exp))))

type ExpressionStack = [ExpressionTree]

-- | Transforms the Forth-AST produced by the Parser into the Target language (python) AST
transpileExpressionTree :: [ForthVal] -> Either ForthErr [ExpressionTree]
transpileExpressionTree [] = Right []
transpileExpressionTree (x : xs) = go [] (x : xs) []
  where
    go [] [] returnstack = Right (Prelude.reverse returnstack)
    go (x : exps) [] returnstack = Right $ Prelude.reverse (x : returnstack)
    go exps (Number a : xs) returnstack = go (Exp (Lit a) : exps) xs returnstack
    go exps (PrintStringLiteral s : zs) returnstack = go exps zs (Prt (PrintLit (Text.unpack s)) : returnstack)
    go exps (Forthvals [Number 0, Mem Retrieve] : zs) returnstack = go (Varname "i" : exps) zs returnstack
    go [] _ _ = Left StackUnderflow
    go (Exp x : xs) (PrintCommand : zs) returnstack = go xs zs (Prt (Print x) : returnstack)
    go (Varname s : xs) (PrintCommand : zs) returnstack = go xs zs (Prt (PrintVar s) : returnstack)
    go exps (Arith Not : xs) returnstack = go (transpileArith exps Not) xs returnstack
    go (x : xs) (If ifvals : zs) returnstack = case lookupTranspile ifvals of
      Left err -> Left err
      Right etree -> go xs zs (Cond (IfExp x etree) : returnstack)
    go (x : xs) (IfElse ifvals elsevals : zs) returnstack = case lookupTranspile ifvals of
      Left err -> Left err
      Right etree -> case lookupTranspile elsevals of
        Left err -> Left err
        Right elsetree -> go xs zs (Cond (IfElseExp x etree elsetree) : returnstack)
    go [x] _ _ = Left StackUnderflow
    go (x : y : xs) (DoLoop (Loop lb) : zs) returnstack = case lookupTranspile lb of
      Left err -> Left err
      Right etree -> go xs zs (Loops (Doloop x y etree) : returnstack)
    go exps (Arith x : xs) returnstack = go (transpileArith exps x) xs returnstack
    go _ _ _ = Left ParseErr

transpileArith :: [ExpressionTree] -> Operator -> [ExpressionTree]
transpileArith (x : y : zs) Add = Exp (Addition y x) : zs
transpileArith (x : y : zs) Sub = Exp (Subtract y x) : zs
transpileArith (x : y : zs) Times = Exp (Multiply y x) : zs
transpileArith (x : y : zs) Div = Exp (IntDiv y x) : zs
transpileArith (x : y : zs) Equal = Exp (Equ y x) : zs
transpileArith (x : y : zs) Less = Exp (Lt y x) : zs
transpileArith (x : y : zs) Greater = Exp (Gt y x) : zs
transpileArith (x : y : zs) And = Exp (AND y x) : zs
transpileArith (x : y : zs) Or = Exp (OR y x) : zs
transpileArith (x : y : zs) Xor = Exp (XOR y x) : zs
transpileArith (x : y : zs) Mod = Exp (MOD y x) : zs
transpileArith (x : zs) Not = Exp (NOT x) : zs
transpileArith [] _ = []
transpileArith _ _ = []

class TargetAST t where
  produceOutput :: t -> String

instance TargetAST ArithExpression where
  produceOutput (Lit x) = show x
  produceOutput x = (Prelude.init . Prelude.tail . show) x

instance TargetAST PrintExpression where
  produceOutput = show

instance TargetAST IfExpression where
  produceOutput = show

instance TargetAST LoopExpression where
  produceOutput = show

instance TargetAST ExpressionTree where
  produceOutput (Exp (Lit x)) = show x
  produceOutput (Exp x) = (Prelude.init . Prelude.tail . show) x
  produceOutput x = show x

generateOutputString :: Either ForthErr ExpressionStack -> String
generateOutputString (Left err) = show err
generateOutputString (Right []) = ""
generateOutputString (Right (x : xs)) = Prelude.unlines $ Prelude.map produceOutput (x : xs)

showOutput :: Either ForthErr [ExpressionTree] -> IO ()
showOutput = putStrLn . generateOutputString

transpileAndOutput :: [ForthVal] -> IO ()
transpileAndOutput = showOutput . transpileExpressionTree

lookupDefs :: ForthVal -> Either ForthErr ForthVal
lookupDefs (Word w) = lookupWord w
  where
    lookupWord w = case M.lookup w initialNames of
      -- to be added here: lookup of variable names and function defs
      Nothing -> Left UnknownWord
      Just i -> case IM.lookup i initialDefs of
        Nothing -> Left UnknownWord
        Just forthval -> Right forthval
lookupDefs f = Right f

lookupTranspile :: [ForthVal] -> Either ForthErr [ExpressionTree]
lookupTranspile vals = traverse lookupDefs vals >>= transpileExpressionTree

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
