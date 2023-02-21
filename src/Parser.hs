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
  , forthValParser'
  , doLoopParser
  , plusLoopParser
  , ifParser
  , ifelseParser
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
  guard
    (firstletter `notElem`
     ["THEN", "ELSE", "IF", "DO", "LOOP", "+LOOP", "CELLS", "ALLOT"])
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

exclamationToken :: Parser Token
exclamationToken = const (Ide "!") <$> (spaces >> char '!' >> spaces)

atToken :: Parser Token
atToken = const (Ide "@") <$> (spaces >> char '@' >> spaces)

commaToken :: Parser Token
commaToken = const COMMA <$> (spaces >> char ',' >> spaces)

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

cellsToken :: Parser Token
cellsToken = const CELLS <$> (spaces *> string "CELLS" <* spaces)

allotToken :: Parser Token
allotToken = const ALLOT <$> (spaces *> string "ALLOT" <* spaces)

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

varToken :: Parser Token
varToken =
  Var . T.pack <$> ((string "VARIABLE" <* spaces) >> (many1 alphaNum <* spaces))

allTokenParser :: Parser Token
allTokenParser =
  try colonToken <|> try operatorToken <|> try semicolonToken <|> try varToken <|>
  try allotToken <|>
  try cellsToken <|>
  try wordToken <|>
  try numToken <|>
  try exclamationToken <|>
  try atToken <|>
  try commaToken <|>
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
  try varToken <|>
  try allotToken <|>
  try cellsToken <|>
  try wordToken <|>
  try numToken <|>
  try (doLoopToken <* loopToken) <|>
  try exclamationToken <|>
  try atToken <|>
  try commaToken <|>
  try unclosedDo

tokensParser :: Parser [Token]
tokensParser = whitespace >> many (tokenParser <* whitespace) <* eof

-- Parser
------------------------------------------------------------------------------------------
-- This is a kind of clumsy hand-written Token parser because Parsec doesn't support this
-- custom Token Type
forthValParser :: [Token] -> Either ForthErr [ForthVal]
forthValParser tokens = forthValParser' tokens [] []

forthValParser' ::
     [Token] -> [ForthVal] -> [ForthVal] -> Either ForthErr [ForthVal]
forthValParser' [] [] parsed = Right $ L.reverse parsed
forthValParser' [] xs _ = Left SyntaxError
forthValParser' (UNCLOSED:xs) _ _ = Left SyntaxError
forthValParser' (Num x:Num y:DOLOOP dotokens:xs) ys parsed =
  doLoopParser x y dotokens xs ys parsed
forthValParser' (Num x:Num y:PLUSLOOP dotokens:xs) ys parsed =
  plusLoopParser x y dotokens xs ys parsed
forthValParser' (Ide text:xs) [] parsed =
  forthValParser' xs [] (Word text : parsed)
forthValParser' (Ide text:xs) ys parsed =
  forthValParser' xs (Word text : ys) parsed
forthValParser' (Num text:xs) [] parsed =
  forthValParser' xs [] (Number (read (T.unpack text)) : parsed)
forthValParser' (Num text:xs) ys parsed =
  forthValParser' xs (Number (read (T.unpack text)) : ys) parsed
forthValParser' (Colon:Ide t:xs) [] parsed = forthValParser' xs [Word t] parsed
forthValParser' (Colon:Operator c:xs) [] parsed =
  forthValParser' xs [Word (T.singleton c)] parsed
forthValParser' (Colon:xs) _ _ = Left SyntaxError
forthValParser' (Operator c:xs) [] parsed =
  forthValParser' xs [] (Word (T.singleton c) : parsed)
forthValParser' (Operator c:xs) ys parsed =
  forthValParser' xs (Word (T.singleton c) : ys) parsed
forthValParser' (Semicolon:xs) [] parsed = Left SyntaxError
forthValParser' (Semicolon:xs) ys parsed = semicolonParser xs ys parsed
forthValParser' (THEN:xs) ys parsed = forthValParser' xs ys parsed
forthValParser' (IF iftokens:xs) ys parsed = ifParser iftokens xs ys parsed
forthValParser' (IFELSE iftokens elsetokens:xs) ys parsed =
  ifelseParser iftokens elsetokens xs ys parsed
forthValParser' (Var name:xs) ys parsed =
  forthValParser' xs ys (Variable name : parsed)
forthValParser' (ALLOT:xs) ys parsed =
  forthValParser' xs ys (Mem Allot : parsed)
forthValParser' (CELLS:xs) ys parsed =
  forthValParser' xs ys (Mem Cellsize : parsed)
forthValParser' (COMMA:xs) ys parsed =
  forthValParser' xs ys (Mem CommaStore : parsed)
forthValParser' _ _ _ = Left SyntaxError

doLoopParser ::
     T.Text
  -> T.Text
  -> [Token]
  -> [Token]
  -> [ForthVal]
  -> [ForthVal]
  -> Either ForthErr [ForthVal]
doLoopParser x y dotokens xs ys parsed =
  case forthValParser dotokens of
    Left err -> Left err
    Right parseresults ->
      forthValParser'
        xs
        ys
        (DoLoop (Loop (read (T.unpack y)) (read (T.unpack x)) parseresults) :
         parsed)

plusLoopParser ::
     T.Text
  -> T.Text
  -> [Token]
  -> [Token]
  -> [ForthVal]
  -> [ForthVal]
  -> Either ForthErr [ForthVal]
plusLoopParser x y dotokens xs ys parsed =
  case forthValParser dotokens of
    Left err -> Left err
    Right parseresults ->
      forthValParser'
        xs
        ys
        (PlusLoop (Loop (read (T.unpack y)) (read (T.unpack x)) parseresults) :
         parsed)

semicolonParser xs ys parsed = forthValParser' xs [] (newdef ys : parsed)
  where
    newdef list =
      Def (Fun (reverseParse (L.last list)) (L.reverse (L.init list)))

ifParser ::
     [Token]
  -> [Token]
  -> [ForthVal]
  -> [ForthVal]
  -> Either ForthErr [ForthVal]
ifParser iftokens xs ys parsed =
  case forthValParser iftokens of
    Left err           -> Left err
    Right parseresults -> forthValParser' xs ys (If parseresults : parsed)

ifelseParser ::
     [Token]
  -> [Token]
  -> [Token]
  -> [ForthVal]
  -> [ForthVal]
  -> Either ForthErr [ForthVal]
ifelseParser iftokens elsetokens xs ys parsed =
  case forthValParser iftokens of
    Left err -> Left err
    Right parseresults ->
      case forthValParser elsetokens of
        Left err -> Left err
        Right parseresultsElse ->
          forthValParser' xs ys (IfElse parseresults parseresultsElse : parsed)

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
