{-# LANGUAGE OverloadedStrings #-}

-- For the moment, lexing and parsing are combined in this module
module Parser
  ( ideToken
  , numToken
  , colonToken
  , semicolonToken
  , tokenParser
  , tokensParser
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
ideToken =
  Ide . T.pack <$>
  ((++) <$> many digit <*> ((++) <$> many1 letter <*> many alphaNum))

numToken :: Parser Token
numToken = Num . T.pack <$> (spaces *> many1 digit <* spaces)

colonToken :: Parser Token
colonToken = const Colon <$> (spaces >> char ':' >> spaces)

semicolonToken :: Parser Token
semicolonToken = const Semicolon <$> (spaces >> char ';' >> spaces)

operatorToken :: Parser Token
operatorToken = Operator <$> (spaces *> oneOf "+-*/" <* spaces)

tokensParser :: Parser [Token]
tokensParser = spaces >> many (tokenParser <* spaces) <* eof

tokenParser :: Parser Token
tokenParser =
  try colonToken <|> try semicolonToken <|> try operatorToken <|> try ideToken <|>
  try numToken
