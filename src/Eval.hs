{-# LANGUAGE OverloadedStrings #-}

module Eval
  ( initialDefs
  , initialEnv
  , initialNames
  , printF
  , eval
  ) where

import           Data.IntMap as IM
import           Data.List   as L
import qualified Data.Map    as Map
import           Data.Text   as T
import           ForthVal

initialNames :: Names
initialNames =
  Map.fromList $
  Prelude.zip ["+", "-", "*", "/", "DUP", "DROP", "SWAP", "OVER", "ROT"] [0 ..]

initialDefs :: Defs
initialDefs =
  IM.fromList $
  Prelude.zip
    [0 ..]
    [ Arith Add
    , Arith Sub
    , Arith Times
    , Arith Div
    , Manip Dup
    , Manip Drop
    , Manip Swap
    , Manip Over
    , Manip Rot
    ]

initialEnv :: Env
initialEnv = Env initialNames initialDefs []

printF :: Env -> IO ()
printF env =
  case stack env of
    []     -> return ()
    (x:xs) -> print x

eval :: Env -> ForthVal -> Either ForthErr Env
eval env (Number x) = Right env {stack = x : stack env}
eval env (Arith op) =
  case stack env of
    [] -> Left StackUnderflow
    [x] -> Left StackUnderflow
    x:y:zs -> Right $ env {stack = operation op y x : zs}
      where operation Add   = (+)
            operation Sub   = (-)
            operation Times = (*)
            operation Div   = div
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

lookupAll text env =
  sequenceA $ Prelude.map (flip Map.lookup (names env)) $ T.split (== ' ') text

evalDefComponents env (Number i)  = Just (Number i)
evalDefComponents env (Word text) = fmap Address $ Map.lookup text (names env)

evalDefBody env forthvals =
  sequenceA $ Prelude.map (evalDefComponents env) forthvals
