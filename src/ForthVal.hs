{-# LANGUAGE OverloadedStrings #-}

module ForthVal
  ( Env(..)
  , ForthVal(..)
  , Operator(..)
  , ForthErr(..)
  , StackManip(..)
  , Names(..)
  , Defs(..)
  ) where

import           Data.IntMap as IM
import qualified Data.Map    as Map
import           Data.Text   as T

data ForthVal
  = Number Int
  | String T.Text
  | Word T.Text
  | Manip StackManip
  | Arith Operator
  | Address Int
  | Def Fun
  deriving (Show, Eq)

data Fun =
  Fun
    { name :: T.Text
    , body :: T.Text
    }
  deriving (Show, Eq)

data Operator
  = Add
  | Sub
  | Times
  | Div
  deriving (Show, Eq)

data StackManip
  = Dup
  | Drop
  | Swap
  | Over
  | Rot
  deriving (Show, Eq)

type Names = Map.Map T.Text Int

type Defs = IntMap [ForthVal]

data Env =
  Env
    { names       :: Names
    , definitions :: Defs
    , stack       :: [Int]
    }
  deriving (Show, Eq)

data ForthErr
  = StackUnderflow
  | UnknownWord
  | InvalidWord
  deriving (Show, Read, Eq)
