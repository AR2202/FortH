{-# LANGUAGE OverloadedStrings #-}

-- For the moment, lexing and parsing are combined in this module
module Parser
  ( ideToken,
    numToken,
    colonToken,
    semicolonToken,
    tokenParser,
    tokensParser,
    forthValParser,
    parseFromText,
    tokenizeFromText,
    forthValParser',
    doLoopParser,
    plusLoopParser,
    ifParser,
    ifelseParser,
  )
where

import Control.Exception (catchJust)
import Control.Monad (guard, void)
import Data.IntMap as IM
import Data.List as L
import qualified Data.Map as Map
import Data.Text as T
import ForthVal
import System.IO.Error (ioeGetErrorType, isDoesNotExistErrorType)
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Language as Lang
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator

-- Lexer
-------------------------------------------------------------------------
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

ideToken :: Parser Token
ideToken =
  Ide . T.pack
    <$> ((++) <$> many digit <*> ((++) <$> many1 letter <*> many alphaNum))

wordToken :: Parser Token
wordToken = do
  whitespace
  firstDigit <- many digit
  firstletter <- many1 letter
  nonFirstLetter <- many alphaNum
  whitespace
  guard
    ( firstletter
        `notElem` ["THEN", "ELSE", "IF", "DO", "LOOP", "+LOOP", "BEGIN", "UNTIL", "CELLS", "ALLOT", "OR", "AND"]
    )
  return $ Ide $ T.pack (firstDigit ++ firstletter ++ nonFirstLetter)

numToken :: Parser Token
numToken = Num . T.pack <$> (spaces *> many1 digit <|> ((++) <$> string "-" <*> many1 digit) <* spaces)

colonToken :: Parser Token
colonToken = const Colon <$> (spaces >> char ':' >> spaces)

semicolonToken :: Parser Token
semicolonToken = const Semicolon <$> (spaces >> char ';' >> spaces)

operatorToken :: Parser Token
operatorToken =
  Operator <$> (spaces *> (oneOf "+-*/=<>" <* notFollowedBy (noneOf " \t\r\n"))) <* (space <|> digit <|> endOfLine <|> newline <|> (eof *> pure ' '))

operatorORToken :: Parser Token
operatorORToken =
  BoolOperator . T.pack <$> (spaces *> (string "OR" <|> string "XOR") <* spaces)

operatorAndToken :: Parser Token
operatorAndToken =
  BoolOperator . T.pack <$> (spaces *> string "AND" <* spaces)

operatorNotToken :: Parser Token
operatorNotToken =
  BoolOperator . T.pack <$> (spaces *> string "NOT" <* spaces)

exclamationToken :: Parser Token
exclamationToken = const (Ide "!") <$> (spaces >> char '!' >> spaces)

atToken :: Parser Token
atToken = const (Ide "@") <$> (spaces >> char '@' >> spaces)

commaToken :: Parser Token
commaToken = const COMMA <$> (spaces >> char ',' >> spaces)

tickToken :: Parser Token
tickToken = const NAME <$> (spaces >> char '\'' >> spaces)

dotToken :: Parser Token
dotToken = const PRINT <$> (spaces >> char '.' >> spaces)

keyToken :: Parser Token
keyToken = KEY <$> (spaces >> string "KEY" >> space *> anyChar <* spaces)

ifToken :: Parser Token
ifToken =
  IF
    <$> between
      (string "IF" <* spaces)
      (lookAhead (try (spaces *> string "THEN")))
      (many allTokenParser)

ifelseToken :: Parser Token
ifelseToken =
  IFELSE
    <$> between
      (string "IF" <* spaces)
      (lookAhead (try (spaces *> string "ELSE")))
      (many allTokenParser)
    <*> between
      (spaces *> string "ELSE" <* spaces)
      (lookAhead (try (spaces *> string "THEN")))
      (many allTokenParser)

unclosedIF :: Parser Token
unclosedIF =
  const UNCLOSED
    <$> ( string "IF" <* spaces
            >> many1 allTokenParser
            >> notFollowedBy (string "THEN")
        )

unclosedELSE :: Parser Token
unclosedELSE =
  const UNCLOSED
    <$> ( string "ELSE" <* spaces
            >> many1 allTokenParser
            >> notFollowedBy (string "THEN")
        )

cellsToken :: Parser Token
cellsToken = const CELLS <$> (spaces *> string "CELLS" <* spaces)

allotToken :: Parser Token
allotToken = const ALLOT <$> (spaces *> string "ALLOT" <* spaces)

typeToken :: Parser Token
typeToken = const TYPE <$> (spaces *> string "TYPE" <* spaces)

thenToken :: Parser Token
thenToken = const THEN <$> (spaces *> string "THEN" <* spaces)

loopToken :: Parser Token
loopToken = const THEN <$> (spaces *> string "LOOP" <* spaces)

ploopToken :: Parser Token
ploopToken = const THEN <$> (spaces *> string "+LOOP" <* spaces)

untilToken :: Parser Token
untilToken = const THEN <$> (spaces *> string "UNTIL" <* spaces)

filePositionToken :: Parser Token
filePositionToken = Ide . T.pack <$> (spaces *> string "FILE-POSITION" <* spaces)

readFileToken :: Parser Token
readFileToken = const READF <$> (spaces *> string "READ-FILE" <* spaces)

doLoopToken :: Parser Token
doLoopToken =
  DOLOOP
    <$> between
      (spaces *> string "DO" <* spaces)
      (lookAhead (spaces *> string "LOOP" <* spaces))
      (many allTokenParser)

untilLoopToken :: Parser Token
untilLoopToken =
  UNTILLOOP
    <$> between
      (spaces *> string "BEGIN" <* spaces)
      (lookAhead (spaces *> string "UNTIL" <* spaces))
      (many allTokenParser)

funToken :: Parser Token
funToken =
  FUN
    <$> between
      (spaces *> char ':' <* spaces)
      ((spaces *> char ';' <* spaces))
      (many allTokenParser)

plusLoopToken :: Parser Token
plusLoopToken =
  PLUSLOOP
    <$> between
      (spaces *> string "DO" <* spaces)
      (lookAhead (try (spaces *> string "+LOOP")))
      (many allTokenParser)

unclosedDo :: Parser Token
unclosedDo =
  const UNCLOSED
    <$> ( string "DO" <* spaces
            >> many1 allTokenParser
            >> notFollowedBy (string "LOOP")
        )

storeStrToken :: Parser Token
storeStrToken =
  STORESTR
    <$> between
      (spaces *> string "S\"" <* spaces)
      (spaces *> char '\"' <* spaces)
      (many (noneOf ['"']))

stringLitToken :: Parser Token
stringLitToken =
  STRING . T.pack
    <$> between
      (char '"')
      (char '"')
      (many (noneOf ['"']))

crToken :: Parser Token
crToken =
  CR <$ (spaces *> string "CR" <* spaces)

varToken :: Parser Token
varToken =
  Var . T.pack <$> ((string "VARIABLE" <* spaces) >> (many1 alphaNum <* spaces))

sourceFileToken :: Parser Token
sourceFileToken =
  EvalSource
    <$> between
      (string ":e \"")
      (char '"')
      (many (noneOf ['"']))

loadFileToken :: Parser Token
loadFileToken =
  LoadSource
    <$> between
      (string ":l \"")
      (char '"')
      (many (noneOf ['"']))

openFileToken :: Parser Token
openFileToken =
  OpenFile
    <$> between
      (string "S\"")
      (string "\" R/W OPEN-FILE" <|> string "\" R/O OPEN-FILE")
      (many (noneOf ['"']))

allTokenParser :: Parser Token
allTokenParser =
  try filePositionToken
    <|> try readFileToken
    <|> try operatorToken
    <|> try operatorORToken
    <|> try operatorAndToken
    <|> try dotToken
    <|> try varToken
    <|> try allotToken
    <|> try cellsToken
    <|> try numToken
    <|> try tickToken
    <|> try (untilLoopToken <* untilToken)
    <|> try crToken
    <|> try openFileToken
    <|> try sourceFileToken
    <|> try storeStrToken
    <|> try stringLitToken
    <|> try keyToken
    <|> try typeToken
    <|> try wordToken
    <|> try exclamationToken
    <|> try atToken
    <|> try commaToken
    <|> try (plusLoopToken <* ploopToken)
    <|> try (doLoopToken <* loopToken)
    <|> try (ifelseToken <* thenToken)
    <|> try (ifToken <* thenToken)
    <|> try unclosedDo
    <|> unclosedIF

tokenParser :: Parser Token
tokenParser =
  try funToken
    <|> try sourceFileToken
    <|> try loadFileToken
    <|> try readFileToken
    <|> try filePositionToken
    <|> try colonToken
    <|> try semicolonToken
    <|> try dotToken
    <|> try tickToken
    <|> try (plusLoopToken <* ploopToken)
    <|> try operatorToken
    <|> try operatorORToken
    <|> try operatorAndToken
    <|> try (ifelseToken <* thenToken)
    <|> try (ifToken <* thenToken)
    <|> try unclosedIF
    <|> try unclosedELSE
    <|> try thenToken
    <|> try varToken
    <|> try allotToken
    <|> try cellsToken
    <|> try (untilLoopToken <* untilToken)
    <|> try crToken
    <|> try openFileToken
    <|> try storeStrToken
    <|> try stringLitToken
    <|> try keyToken
    <|> try typeToken
    <|> try wordToken
    <|> try numToken
    <|> try (doLoopToken <* loopToken)
    <|> try exclamationToken
    <|> try atToken
    <|> try commaToken
    <|> try unclosedDo

tokensParser :: Parser [Token]
tokensParser = whitespace >> many (tokenParser <* whitespace) <* eof

-- Parser
------------------------------------------------------------------------------------------
-- This is a kind of clumsy hand-written Token parser because Parsec doesn't support this
-- custom Token Type
forthValParser :: [Token] -> Either ForthErr [ForthVal]
forthValParser tokens = forthValParser' tokens []

forthValParser' ::
  [Token] -> [ForthVal] -> Either ForthErr [ForthVal]
forthValParser' [] parsed = Right $ L.reverse parsed
forthValParser' (FUN funtokens : xs) parsed =
  funParser funtokens xs parsed
forthValParser' (UNCLOSED : xs) _ = Left SyntaxError
forthValParser' (DOLOOP dotokens : xs) parsed =
  doLoopParser dotokens xs parsed
forthValParser' (PLUSLOOP dotokens : xs) parsed =
  plusLoopParser dotokens xs parsed
forthValParser' (UNTILLOOP dotokens : xs) parsed =
  untilLoopParser dotokens xs parsed
forthValParser' (NAME : (Ide t) : xs) parsed =
  forthValParser' xs (NameLookup t : parsed)
forthValParser' (READF : xs) parsed =
  forthValParser' xs (ReadFile : parsed)
forthValParser' (Ide "RECURSE" : xs) parsed =
  forthValParser' xs (Recurse : parsed)
forthValParser' (Ide text : xs) parsed =
  forthValParser' xs (Word text : parsed)
forthValParser' (Num text : xs) parsed =
  forthValParser' xs (Number (read (T.unpack text)) : parsed)
forthValParser' (Colon : xs) _ = Left SyntaxError
forthValParser' (Operator c : xs) parsed =
  forthValParser' xs (Word (T.singleton c) : parsed)
forthValParser' (BoolOperator c : xs) parsed =
  forthValParser' xs (Word c : parsed)
forthValParser' (Semicolon : xs) parsed = Left SyntaxError
forthValParser' (THEN : xs) parsed = forthValParser' xs parsed
forthValParser' (IF iftokens : xs) parsed = ifParser iftokens xs parsed
forthValParser' (IFELSE iftokens elsetokens : xs) parsed =
  ifelseParser iftokens elsetokens xs parsed
forthValParser' (Var name : xs) parsed =
  forthValParser' xs (Variable name : parsed)
forthValParser' (ALLOT : xs) parsed =
  forthValParser' xs (Mem Allot : parsed)
forthValParser' (CELLS : xs) parsed =
  forthValParser' xs (Mem Cellsize : parsed)
forthValParser' (COMMA : xs) parsed =
  forthValParser' xs (Mem CommaStore : parsed)
forthValParser' (CR : xs) parsed =
  forthValParser' xs (PrintStringLiteral "\n" : parsed)
forthValParser' (PRINT : STRING t : xs) parsed =
  forthValParser' xs (PrintStringLiteral t : parsed)
forthValParser' (PRINT : xs) parsed =
  forthValParser' xs (PrintCommand : parsed)
forthValParser' (KEY c : xs) parsed =
  forthValParser' xs (Key c : parsed)
forthValParser' (TYPE : xs) parsed =
  forthValParser' xs (Type : parsed)
forthValParser' (STORESTR s : xs) parsed =
  forthValParser' xs (StoreString s : parsed)
forthValParser' (EvalSource s : xs) parsed =
  forthValParser' xs (SourceFile s : parsed)
forthValParser' (LoadSource s : xs) parsed =
  forthValParser' xs (Load s : parsed)
forthValParser' (OpenFile s : xs) parsed =
  forthValParser' xs (Forthvals [Number 0, Mem StoreNext, Number (L.length s), Mem StoreNext, StoreString s, Manip Drop, Manip Drop, Manip Drop, Manip Swap] : parsed)
forthValParser' _ _ = Left SyntaxError

doLoopParser ::
  [Token] ->
  [Token] ->
  [ForthVal] ->
  Either ForthErr [ForthVal]
doLoopParser dotokens xs parsed =
  case forthValParser dotokens of
    Left err -> Left err
    Right parseresults ->
      forthValParser'
        xs
        ( DoLoop (Loop parseresults)
            : parsed
        )

untilLoopParser ::
  [Token] ->
  [Token] ->
  [ForthVal] ->
  Either ForthErr [ForthVal]
untilLoopParser dotokens xs parsed =
  case forthValParser dotokens of
    Left err -> Left err
    Right parseresults ->
      forthValParser'
        xs
        ( UntilLoop (Loop parseresults)
            : parsed
        )

plusLoopParser ::
  [Token] ->
  [Token] ->
  [ForthVal] ->
  Either ForthErr [ForthVal]
plusLoopParser dotokens xs parsed =
  case forthValParser dotokens of
    Left err -> Left err
    Right parseresults ->
      forthValParser'
        xs
        ( PlusLoop (Loop parseresults)
            : parsed
        )

ifParser ::
  [Token] ->
  [Token] ->
  [ForthVal] ->
  Either ForthErr [ForthVal]
ifParser iftokens xs parsed =
  case forthValParser iftokens of
    Left err -> Left err
    Right parseresults -> forthValParser' xs (If parseresults : parsed)

ifelseParser ::
  [Token] ->
  [Token] ->
  [Token] ->
  [ForthVal] ->
  Either ForthErr [ForthVal]
ifelseParser iftokens elsetokens xs parsed =
  case forthValParser iftokens of
    Left err -> Left err
    Right parseresults ->
      case forthValParser elsetokens of
        Left err -> Left err
        Right parseresultsElse ->
          forthValParser' xs (IfElse parseresults parseresultsElse : parsed)

funParser :: [Token] -> [Token] -> [ForthVal] -> Either ForthErr [ForthVal]
funParser funtokens xs parsed =
  case forthValParser funtokens of
    Left err -> Left err
    Right (Word t : parseresults) -> forthValParser' xs (Def (Fun t parseresults) : parsed)
    _ -> Left InvalidWord

tokenizeFromText :: String -> T.Text -> Either ParseError [Token]
tokenizeFromText = parse tokensParser

parseFromText :: String -> T.Text -> Either ForthErr [ForthVal]
parseFromText filename text =
  case tokenizeFromText filename text of
    Left _ -> Left ParseErr
    Right tokenslist -> forthValParser tokenslist
