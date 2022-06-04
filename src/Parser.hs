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

-- Lexer
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

tokenParser :: Parser Token
tokenParser =
  try colonToken <|> try semicolonToken <|> try operatorToken <|> try ideToken <|>
  try numToken

tokensParser :: Parser [Token]
tokensParser = spaces >> many (tokenParser <* spaces) <* eof

-- Parser
-- This is a kind of clumsy hand-written Token parser because Parsec doesn't support this
-- custom Token Type
forthValParser :: [Token] -> Either ForthErr [ForthVal]
forthValParser tokens = go tokens [] []
  where
    go [] [] parsed = Right $ L.reverse parsed
    go [] xs _ = Left SyntaxError
    go (Ide text:xs) [] parsed = go xs [] (Word text : parsed)
    go (Ide text:xs) ys parsed = go xs (Word text : ys) parsed
    go (Num text:xs) [] parsed =
      go xs [] (Number (read (T.unpack text)) : parsed)
    go (Num text:xs) ys parsed =
      go xs (Number (read (T.unpack text)) : ys) parsed
    go (Colon:Ide t:xs) [] parsed = go xs [Word t] parsed
    go (Colon:Operator c:xs) [] parsed = go xs [Word (T.singleton c)] parsed
    go (Colon:xs) _ _ = Left SyntaxError
    go (Operator c:xs) [] parsed = go xs [] (Word (T.singleton c) : parsed)
    go (Operator c:xs) ys parsed = go xs (Word (T.singleton c) : ys) parsed
    go (Semicolon:xs) [] parsed = Left SyntaxError
    go (Semicolon:xs) ys parsed = go xs [] (newdef ys : parsed)
      where
        newdef list = Def (Fun (reverseParse (L.last list)) (L.init list))

-- this function is partial. However, it should never be called on a ForthVal Variant other than Word
reverseParse :: ForthVal -> T.Text
reverseParse (Word t) = t
