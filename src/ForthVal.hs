{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ForthVal
  ( Env(..)
  , ForthVal(..)
  , Operator(..)
  , ForthErr(..)
  , StackManip(..)
  , Names(..)
  , Defs(..)
  , Fun(..)
  , Token(..)
  ) where

import           Data.IntMap                       as IM
import qualified Data.Map                          as Map
import           Data.Text                         as T
import           Generic.Random                    (genericArbitrary', uniform)
import           GHC.Generics
import           Test.QuickCheck                   (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic

data ForthVal
  = Number Int
  | Word T.Text
  | Manip StackManip
  | Arith Operator
  | Address [Int]
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
  deriving (Show, Eq, Generic)

instance Arbitrary Operator where
  arbitrary = genericArbitrary
  shrink = genericShrink

data StackManip
  = Dup
  | Drop
  | Swap
  | Over
  | Rot
  deriving (Show, Eq)

type Names = Map.Map T.Text Int

type Defs = IntMap ForthVal

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

data Token
  = Ide T.Text
  | Num T.Text
  | Operator Char
  | Colon
  | Semicolon
  deriving (Show, Eq)
