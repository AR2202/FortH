{-# LANGUAGE OverloadedStrings #-}

-- For the moment, lexing and parsing are combined in this module
module Parser
  (
  ) where

import           Data.IntMap                        as IM
import           Data.List                          as L
import qualified Data.Map                           as Map
import           Data.Text                          as T
import           ForthVal
import qualified Text.Parsec.Language               as Lang

import           Text.Parsec
import           Text.Parsec.Expr
import qualified Text.Parsec.Language               as Lang
import           Text.Parsec.Text
import qualified Text.Parsec.Token                  as Tok
import           Text.ParserCombinators.Parsec.Char

ideToken :: Parser Token
ideToken = Ide . T.pack <$> (many1 space >> many1 letter >> many1 space)

numToken :: Parser Token
numToken = Num . T.pack <$> (many1 space >> many1 digit >> many1 space)

colonToken :: Parser Token
colonToken = const Colon <$> (many1 space >> char ':' >> many1 space)

semicolonToken :: Parser Token
semicolonToken = const Semicolon <$> (many1 space >> char ';' >> many1 space)
