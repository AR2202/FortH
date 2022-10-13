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
-------------------------------------------------------------------------
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
  guard (firstletter `notElem` ["THEN", "ELSE", "IF", "DO", "LOOP", "+LOOP"])
  return $ Ide $ T.pack (firstDigit ++ firstletter ++ nonFirstLetter)

numToken :: Parser Token
numToken = Num . T.pack <$> (spaces *> many1 digit <* spaces)

colonToken :: Parser Token
colonToken = const Colon <$> (spaces >> char ':' >> spaces)

semicolonToken :: Parser Token
semicolonToken = const Semicolon <$> (spaces >> char ';' >> spaces)

operatorToken :: Parser Token
operatorToken =
  Operator <$> (spaces *> oneOf "+-*/=<>" <* (space <|> (lookAhead digit)))

ifToken :: Parser Token
ifToken =
  IF <$>
  between
    (string "IF" <* spaces)
    (lookAhead (try (string "THEN")))
    (many allTokenParser)

ifelseToken :: Parser Token
ifelseToken =
  IFELSE <$>
  between
    (string "IF" <* spaces)
    (lookAhead (try (string "ELSE")))
    (many allTokenParser) <*>
  between
    (string "ELSE" <* spaces)
    (lookAhead (try (string "THEN")))
    (many allTokenParser)

unclosedIF :: Parser Token
unclosedIF =
  const UNCLOSED <$>
  (string "IF" <* spaces >> many1 allTokenParser >>
   notFollowedBy (string "THEN"))

unclosedELSE :: Parser Token
unclosedELSE =
  const UNCLOSED <$>
  (string "ELSE" <* spaces >> many1 allTokenParser >>
   notFollowedBy (string "THEN"))

thenToken :: Parser Token
thenToken = const THEN <$> (spaces *> string "THEN" <* spaces)

loopToken :: Parser Token
loopToken = const THEN <$> (spaces *> string "LOOP" <* spaces)

ploopToken :: Parser Token
ploopToken = const THEN <$> (spaces *> string "+LOOP" <* spaces)

doLoopToken :: Parser Token
doLoopToken =
  DOLOOP <$>
  between
    (string "DO" <* spaces)
    (lookAhead (string "LOOP"))
    (many allTokenParser)

plusLoopToken :: Parser Token
plusLoopToken =
  PLUSLOOP <$>
  between
    (string "DO" <* spaces)
    (lookAhead (try (string "+LOOP")))
    (many allTokenParser)

unclosedDo :: Parser Token
unclosedDo =
  const UNCLOSED <$>
  (string "DO" <* spaces >> many1 allTokenParser >>
   notFollowedBy (string "LOOP"))

allTokenParser :: Parser Token
allTokenParser =
  try colonToken <|> try operatorToken <|> try semicolonToken <|> try wordToken <|>
  try numToken <|>
  try (plusLoopToken <* ploopToken) <|>
  try (doLoopToken <* loopToken) <|>
  try (ifelseToken <* thenToken) <|>
  try (ifToken <* thenToken) <|>
  try unclosedDo <|>
  unclosedIF

tokenParser :: Parser Token
tokenParser =
  try colonToken <|> try semicolonToken <|> try (plusLoopToken <* ploopToken) <|>
  try operatorToken <|>
  try (ifelseToken <* thenToken) <|>
  try (ifToken <* thenToken) <|>
  try unclosedIF <|>
  try unclosedELSE <|>
  try thenToken <|>
  try wordToken <|>
  try numToken <|>
  try (doLoopToken <* loopToken) <|>
  try unclosedDo

tokensParser :: Parser [Token]
tokensParser = whitespace >> many (tokenParser <* whitespace) <* eof

-- Parser
------------------------------------------------------------------------------------------
-- This is a kind of clumsy hand-written Token parser because Parsec doesn't support this
-- custom Token Type
-- todo: parsing if else
forthValParser :: [Token] -> Either ForthErr [ForthVal]
forthValParser tokens = go tokens [] []
  where
    go [] [] parsed = Right $ L.reverse parsed
    go [] xs _ = Left SyntaxError
    go (UNCLOSED:xs) _ _ = Left SyntaxError
    go (Num x:Num y:DOLOOP dotokens:xs) ys parsed =
      case forthValParser dotokens of
        Left err -> Left err
        Right parseresults ->
          go
            xs
            ys
            (DoLoop (Loop (read (T.unpack y)) (read (T.unpack x)) parseresults) :
             parsed)
    go (Num x:Num y:PLUSLOOP dotokens:xs) ys parsed =
      case forthValParser dotokens of
        Left err -> Left err
        Right parseresults ->
          go
            xs
            ys
            (PlusLoop
               (Loop (read (T.unpack y)) (read (T.unpack x)) parseresults) :
             parsed)
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
    go (THEN:xs) ys parsed = go xs ys parsed
    go (IF iftokens:xs) ys parsed =
      case forthValParser iftokens of
        Left err           -> Left err
        Right parseresults -> go xs ys (If parseresults : parsed)
    go (IFELSE iftokens elsetokens:xs) ys parsed =
      case forthValParser iftokens of
        Left err -> Left err
        Right parseresults ->
          case forthValParser elsetokens of
            Left err -> Left err
            Right parseresultsElse ->
              go xs ys (IfElse parseresults parseresultsElse : parsed)
    go _ _ _ = Left SyntaxError

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
