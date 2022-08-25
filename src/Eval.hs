{-# LANGUAGE OverloadedStrings #-}

module Eval
  ( initialDefs
  , initialEnv
  , initialNames
  , printF
  , printStack
  , eval
  , evalAndPrintStackTop
  , evalInput
  , evalAndPrintStackTopRepl
  , evalInputRepl
  , evalFile
  ) where

import           Data.IntMap as IM
import           Data.List   as L
import qualified Data.Map    as Map
import           Data.Text   as T
import           ForthVal
import           Parser

initialNames :: Names
initialNames =
  Map.fromList $
  Prelude.zip
    [ "+"
    , "-"
    , "*"
    , "/"
    , "="
    , "<"
    , ">"
    , "DUP"
    , "DROP"
    , "SWAP"
    , "OVER"
    , "ROT"
    , "THEN"
    ]
    [0 ..]

initialDefs :: Defs
initialDefs =
  IM.fromList $
  Prelude.zip
    [0 ..]
    [ Arith Add
    , Arith Sub
    , Arith Times
    , Arith Div
    , Arith Equal
    , Arith Less
    , Arith Greater
    , Manip Dup
    , Manip Drop
    , Manip Swap
    , Manip Over
    , Manip Rot
    , Manip Drop
    ]

initialEnv :: Env
initialEnv = Env initialNames initialDefs []

printF :: Env -> IO ()
printF env =
  case stack env of
    []     -> return ()
    (x:xs) -> print x

printStack :: Env -> IO ()
printStack env = mapM_ print $ L.reverse $ stack env

eval :: Env -> ForthVal -> Either ForthErr Env
eval env (Number x) = Right env {stack = x : stack env}
eval env (Arith op) =
  case stack env of
    [] -> Left StackUnderflow
    [x] -> Left StackUnderflow
    x:y:zs -> Right $ env {stack = operation op y x : zs}
      where operation Add = (+)
            operation Sub = (-)
            operation Times = (*)
            operation Div = div
            operation Equal =
              \x y ->
                if x == y
                  then 1
                  else 0
            operation Less =
              \x y ->
                if x < y
                  then 1
                  else 0
            operation Greater =
              \x y ->
                if x > y
                  then 1
                  else 0
eval env (Manip Dup) =
  case stack env of
    []     -> Left StackUnderflow
    (x:xs) -> Right $ env {stack = x : x : xs}
eval env (Manip Drop) =
  case stack env of
    []     -> Left StackUnderflow
    (x:xs) -> Right $ env {stack = xs}
eval env (Manip Swap) =
  case stack env of
    []     -> Left StackUnderflow
    [x]    -> Left StackUnderflow
    x:y:zs -> Right $ env {stack = y : x : zs}
eval env (Manip Over) =
  case stack env of
    []     -> Left StackUnderflow
    [x]    -> Left StackUnderflow
    x:y:zs -> Right $ env {stack = x : y : x : zs}
eval env (Manip Rot) =
  case stack env of
    []       -> Left StackUnderflow
    [x]      -> Left StackUnderflow
    x:[y]    -> Left StackUnderflow
    x:y:z:zs -> Right $ env {stack = y : z : x : zs}
eval env (Word name) =
  case Map.lookup name (names env) of
    Nothing -> Left UnknownWord
    Just i ->
      case IM.lookup i (definitions env) of
        Nothing       -> Left UnknownWord
        Just forthval -> eval env forthval
eval env (Address i) =
  case IM.lookup i (definitions env) of
    Nothing       -> Left UnknownWord
    Just forthval -> eval env forthval
eval env (Def fun) =
  case evalDefBody env (body fun) of
    Nothing -> Left UnknownWord
    Just forthvals ->
      Right $
      env
        { names = Map.insert newword newaddress (names env)
        , definitions =
            IM.insert newaddress (Forthvals forthvals) (definitions env)
        }
      where newword = name fun
            newaddress = Map.size (names env)
eval env (Forthvals forthvals) = L.foldl' eval' (Right env) forthvals
  where
    eval' eEnv forthval =
      case eEnv of
        Left error -> Left error
        Right env  -> eval env forthval
eval env (If forthvals) =
  case stack env of
    []   -> Left StackUnderflow
    0:xs -> Right env
    _    -> eval env (Forthvals forthvals)
eval env (Else forthvals) =
  case stack env of
    []   -> Left StackUnderflow
    1:xs -> Right env
    _    -> eval env (Forthvals forthvals)

lookupAll text env =
  sequenceA $ Prelude.map (flip Map.lookup (names env)) $ T.split (== ' ') text

evalDefComponents env (Number i)  = Just (Number i)
evalDefComponents env (Word text) = fmap Address $ Map.lookup text (names env)

evalDefBody env forthvals =
  sequenceA $ Prelude.map (evalDefComponents env) forthvals

evalInput :: String -> T.Text -> Env -> Either ForthErr Env
evalInput filename text env =
  case parseFromText filename text of
    Left e -> Left e
    Right forthvals -> L.foldl' eval' (Right env) forthvals
      where eval' eEnv forthval =
              case eEnv of
                Left error -> Left error
                Right env  -> eval env forthval

evalInputRepl :: T.Text -> Env -> Either ForthErr Env
evalInputRepl = evalInput "repl"

evalAndPrintStackTop :: String -> T.Text -> Env -> IO ()
evalAndPrintStackTop filename text env =
  mapM_ printF $ evalInput filename text env

evalAndPrintStackTopRepl = evalAndPrintStackTop "repl"

evalFile :: String -> IO ()
evalFile filename = do
  contents <- readFile filename
  let text = T.unwords $ T.lines $ T.pack contents
  evalAndPrintStackTop filename text initialEnv
