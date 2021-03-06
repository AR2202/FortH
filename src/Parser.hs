{-# LANGUAGE OverloadedStrings #-}

-- For the moment, lexing and parsing are combined in this module
module Parser
  ( ideToken
  , numToken
  , colonToken
  , semicolonToken
  , tokenParser
  , tokensParser
  , parseFromText
  , tokenizeFromText
  ) where

import           Data.IntMap                              as IM
import           Data.List                                as L
import qualified Data.Map                                 as Map
import           Data.Text                                as T
import           ForthVal

import           Text.Parsec
import           Text.Parsec.Expr
import qualified Text.Parsec.Language                     as Lang

import           Control.Monad                            (guard, void)
import           Text.Parsec.Text
import qualified Text.Parsec.Token                        as Tok
import           Text.ParserCombinators.Parsec.Char
import           Text.ParserCombinators.Parsec.Combinator

-- Lexer
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

ideToken :: Parser Token
ideToken =
  Ide . T.pack <$>
  ((++) <$> many digit <*> ((++) <$> many1 letter <*> many alphaNum))

wordToken :: Parser Token
wordToken = do
  whitespace
  firstDigit <- many digit
  firstletter <- many1 letter
  nonFirstLetter <- many alphaNum
  whitespace
  guard (firstletter `notElem` ["THEN", "ELSE", "IF"])
  return $ Ide $ T.pack (firstDigit ++ firstletter ++ nonFirstLetter)

numToken :: Parser Token
numToken = Num . T.pack <$> (spaces *> many1 digit <* spaces)

colonToken :: Parser Token
colonToken = const Colon <$> (spaces >> char ':' >> spaces)

semicolonToken :: Parser Token
semicolonToken = const Semicolon <$> (spaces >> char ';' >> spaces)

operatorToken :: Parser Token
operatorToken = Operator <$> (spaces *> oneOf "+-*/=<>" <* spaces)

ifToken :: Parser Token
ifToken =
  IF <$>
  between (string "IF") (lookAhead (try (string "THEN"))) (many allTokenParser)

ifelseToken :: Parser Token
ifelseToken =
  IF <$>
  between (string "IF") (lookAhead (try (string "ELSE"))) (many allTokenParser)

-- this is not yet good as this doesn't distinguish between empty if blocks and syntax error
unclosedIF :: Parser Token
unclosedIF =
  const (IF []) <$>
  (string "IF" >> many1 allTokenParser >> notFollowedBy (string "THEN"))

unclosedELSE :: Parser Token
unclosedELSE =
  const (ELSE []) <$>
  (string "ELSE" >> many1 allTokenParser >> notFollowedBy (string "THEN"))

elseToken :: Parser Token
elseToken =
  ELSE <$>
  between
    (string "ELSE")
    (lookAhead (try (string "THEN")))
    (many allTokenParser)

thenToken :: Parser Token
thenToken = const THEN <$> (spaces *> string "THEN" <* spaces)

-- can't yet parse nested if or else statements
allTokenParser :: Parser Token
allTokenParser =
  try colonToken <|> try operatorToken <|> try semicolonToken <|> try wordToken <|>
  try numToken <|>
  ifToken

tokenParser :: Parser Token
tokenParser =
  try colonToken <|> try semicolonToken <|> try operatorToken <|>
  try ifelseToken <|>
  try elseToken <|>
  try ifToken <|>
  try unclosedIF <|>
  try unclosedELSE <|>
  try thenToken <|>
  try wordToken <|>
  try numToken

tokensParser :: Parser [Token]
tokensParser = whitespace >> many (tokenParser <* whitespace) <* eof

-- Parser
-- This is a kind of clumsy hand-written Token parser because Parsec doesn't support this
-- custom Token Type
-- todo: parsing if else
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
        newdef list =
          Def (Fun (reverseParse (L.last list)) (L.reverse (L.init list)))

-- this function is partial. However, it should never be called on a ForthVal Variant other than Word
reverseParse :: ForthVal -> T.Text
reverseParse (Word t) = t

tokenizeFromText :: String -> T.Text -> Either ParseError [Token]
tokenizeFromText = parse tokensParser

parseFromText :: String -> T.Text -> Either ForthErr [ForthVal]
parseFromText filename text =
  case tokenizeFromText filename text of
    Left _           -> Left ParseErr
    Right tokenslist -> forthValParser tokenslist
