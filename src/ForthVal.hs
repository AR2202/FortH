{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ForthVal
  ( Env (..),
    ForthVal (..),
    Operator (..),
    ForthErr (..),
    StackManip (..),
    MemoryOp (..),
    Names (..),
    Defs (..),
    Fun (..),
    Token (..),
    Loop (..),
  )
where

import Data.IntMap as IM
import qualified Data.Map as Map
import Data.Text as T
import qualified Data.Vector as V
import GHC.Generics
import Generic.Random (genericArbitrary', uniform)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Arbitrary.Generic

data ForthVal
  = Number Int
  | Word T.Text
  | Manip StackManip
  | Arith Operator
  | Address Int
  | Forthvals [ForthVal]
  | Def Fun
  | If [ForthVal]
  | IfElse [ForthVal] [ForthVal]
  | DoLoop Loop
  | PlusLoop Loop
  | UntilLoop Loop
  | Variable T.Text
  | Mem MemoryOp
  | PrintCommand
  | PrintStringLiteral T.Text
  | DictLookup
  | NameLookup T.Text
  | Ascii
  | Key Char
  | Type
  | StoreString String
  | SourceFile String
  | ReadFile
  | Recurse
  deriving (Show, Eq)

newtype Loop = Loop
  { loopbody :: [ForthVal]
  }
  deriving (Show, Eq)

data Fun = Fun
  { name :: T.Text,
    body :: [ForthVal]
  }
  deriving (Show, Eq)

data Operator
  = Add
  | Sub
  | Times
  | Div
  | Equal
  | Less
  | Greater
  | Or
  | And
  | Xor
  | Not
  | Mod
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
  | Invert
  deriving (Show, Eq)

data MemoryOp
  = Store
  | Retrieve
  | Allot
  | Cellsize
  | CommaStore
  | StoreNext
  deriving (Show, Eq)

type Names = Map.Map T.Text Int

type Defs = IntMap ForthVal

data Env = Env
  { names :: Names,
    definitions :: Defs,
    stack :: [Int],
    mem :: IntMap Int,
    memorycell :: Int,
    printStr :: [String]
  }
  deriving (Show, Eq)

data ForthErr
  = StackUnderflow
  | UnknownWord
  | InvalidWord
  | SyntaxError
  | ParseErr
  | MemoryAccessError
  | NonAsciiCode
  | FileNotFound String
  deriving (Show, Read, Eq)

data Token
  = Ide T.Text
  | Num T.Text
  | Operator Char
  | BoolOperator T.Text
  | Colon
  | Semicolon
  | IF [Token]
  | IFELSE [Token] [Token]
  | THEN
  | UNCLOSED
  | ALLOT
  | CELLS
  | DOLOOP [Token]
  | PLUSLOOP [Token]
  | UNTILLOOP [Token]
  | Var T.Text
  | COMMA
  | PRINT
  | STRING T.Text
  | FUN [Token]
  | NAME
  | KEY Char
  | TYPE
  | STORESTR String
  | EvalSource String
  | OpenFile String
  | READF
  deriving (Show, Eq)
