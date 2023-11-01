{-# LANGUAGE OverloadedStrings #-}
-- import           Text.Parsec.Expr
{-# LANGUAGE OverloadedStrings #-}

-- import           Text.Parsec.Expr
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.IntMap as IM
import Data.List as L
import qualified Data.Map as Map
import Data.Text as T
import Eval
import ForthVal
import Parser
import Test.Hspec
import Test.QuickCheck
import Text.Parsec
import qualified Text.Parsec.Language as Lang
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Char
import Transpiler

main :: IO ()
main =
  hspec $
    do
      -- Tests for environment initial setup
      -----------------------------------------
      propInitialStack

      environmentInitialDef

      -- Tokenizing Tests
      --------------------
      -- Arithmetic
      tokenizeAddition
      -- Ide
      tokenizeIdeWithNum
      tokenizenumWithIde
      -- loop
      tokenizeLoop
      -- if
      tokenizeDoLoopWithIF

      -- Parsing Tests
      ----------
      -- Arithmetic
      parseAdd
      -- identifiers
      parseIdentifier
      -- loop
      parseDoLoopWithI
      parsePLoopWithI
      parseUnclosedLoop
      parseUntilLoop
      parseErrUntilLoop
      -- Definitions
      parseDefWord
      parseIncompleteDef
      parseSingleSemicolon
      parseifInsideDef
      parseRecInsideDef
      -- IF
      parseifVals
      parseLoopInsideIf
      parseIFInsideIf
      parseIFInsideElse
      parseDoLoopWithIF
      parsePLoopWithIF
      parseIFElseInsideElse

      -- Tree-walk Interpreter (Eval) Tests
      -----------------------------
      -- Arithmetic
      evalMultiply
      evalAdd
      -- Stack manipulations
      evalDup
      evalOver
      evalDrop
      evalDupExeption
      -- Definitions
      evalDefinitions
      evalUndefinedError
      evalNewWord
      -- Execution Token
      evalExecutionToken
      evalDictLookupEqAddress
      evalNameLookup
      evalNameLookupUnkownError
      -- evaluating many Forth expressions
      evalManyForthExpressions
      -- If Else
      evalIfExecutedIfTrue
      evalIFNotExecutedIfFalse
      evalElseExecuted
      -- Loops
      evalDooLoopnTimes
      evalDoLoopExecuted
      evalDoLoopNotExecuted
      evalIndexInLoop
      evalPlusLoopExecuted
      evalPlusLoopIncreasesIndex
      evalUntilLoopUntilTrue
      evalUntilLoopStopCondition
      -- Variable assignment Tests
      evalUninitializedException
      evalInitializeVar
      evalAssignVar
      -- Memory operation Tests
      evalStoreRetrieve
      evalCellAllot
      evalCommaStore
      evalCommaStoreTwice
      evalCommaStoreDump
      errorOnAccessingUninitializedMemory
      errorOnAccessingMemoryWithoutAddress
      errorOnDumpUninitializedMemory
      -- print
      evalPrintRemovedFromStack
      evalPrintAppendedToPrintStr
      evalPrintStringLitAppendedToPrintStr
      -- ASCII
      evalAsciiAppendedToPrintStr
      evalNonAscii
      evalAsciiCodeAOnStack
      -- Strings
      evalStoreAndTypeString
      -- Recursion
      recurseIfTrue
      -- evalT Monad transformer
      evalSourceDoesNotExist
      evalSource
      evalTwithPureValue
      -- Tests for Transpiler
      ------------------------
      transpileAddAndSubtract
      transpilePrintExpression
      transpilePrintStringLiteral
      transpileMod
      transpilenestedIf
      transpilenestedIfElse
      transpileifGreater

---------Test for initial environment----------
-----------------------------------------------
-- Unit tests----------

initialDefs_addition_operator :: Bool
initialDefs_addition_operator =
  case Map.lookup "+" (_names initialEnv) of
    Nothing -> False
    Just i ->
      case IM.lookup i (_definitions initialEnv) of
        Nothing -> False
        Just forthval -> forthval == Arith Add

-- property based tests----------

-- | test that initially the stack is empty
-- and binary operators have a stack underflow error
prop_initial_stack_empty :: Operator -> Bool
prop_initial_stack_empty op = eval initialEnv (Arith op) == Left StackUnderflow

propInitialStack :: SpecWith ()
propInitialStack = describe "initialEnv" $ do
  it "should start with an empty stack" $ property prop_initial_stack_empty

-----------------------------------------
-----------Tests for tokinizer/lexing----
-----------------------------------------
tokenizeAdd :: Expectation
tokenizeAdd =
  tokenizeFromText "test" "1 2 +"
    `shouldBe` Right [Num "1", Num "2", Operator '+']

tokenizeAddition :: SpecWith ()
tokenizeAddition =
  describe "tokenizer" $
    context "when tokenizing an addition expression" $
      it
        "should return Num and Operator Tokens"
        tokenizeAdd

tokenizeDoLoop :: Expectation
tokenizeDoLoop =
  tokenizeFromText "test" "10 1 DO I . LOOP"
    `shouldBe` Right [Num "10", Num "1", DOLOOP [Ide "I", PRINT]]

tokenizeLoop :: SpecWith ()
tokenizeLoop =
  describe "tokenizer" $
    context "when tokenizing a do loop" $
      it
        "should return Doloop Token and loop body"
        tokenizeDoLoop

ifInsidedoLoopToken :: Expectation
ifInsidedoLoopToken =
  tokenizeFromText "test" "10 1 DO I 3 = IF .\"equal to 3\" THEN  LOOP"
    `shouldBe` Right
      [ Num "10",
        Num "1",
        DOLOOP [Ide "I", Num "3", Operator '=', IF [PRINT, STRING "equal to 3"]]
      ]

tokenizeDoLoopWithIF :: SpecWith ()
tokenizeDoLoopWithIF =
  describe "tokenizer" $
    context "when lexing a Do Loop with an If statement" $
      it
        "should tokenize it as a loop with the if in the body"
        ifInsidedoLoopToken

ideWithNum :: Expectation
ideWithNum =
  tokenizeFromText "test" "var10 var6 v3var"
    `shouldBe` Right [Ide "var10", Ide "var6", Ide "v3var"]

tokenizeIdeWithNum :: SpecWith ()
tokenizeIdeWithNum =
  describe "tokenizer" $
    context "when lexing a word with a number in it" $
      it
        "should tokenize it as an identifier and not a number"
        ideWithNum

ideStartsWithNum :: Expectation
ideStartsWithNum =
  tokenizeFromText "test" "10var 10+"
    `shouldBe` Right [Ide "10var", Num "10", Operator '+']

tokenizenumWithIde :: SpecWith ()
tokenizenumWithIde =
  describe "tokenizer" $
    context "when lexing a word with a number at the beginning" $
      it
        "should tokenize it as  an identifier"
        ideStartsWithNum

------------------------------------
-----------Tests for parsing----------
--------------------------------------
parseIdExists :: Expectation
parseIdExists =
  parse ideToken "file" "aword"
    `shouldBe` Right (Ide "aword")

parseIdentifier :: SpecWith ()
parseIdentifier =
  describe "ideToken" $
    it
      "should return an identifier Token"
      parseIdExists

tokenizeAndParseAdd :: Expectation
tokenizeAndParseAdd =
  parseFromText "test" "1 2 +"
    `shouldBe` Right [Number 1, Number 2, Word "+"]

parseAdd :: SpecWith ()
parseAdd =
  describe "forthValParser" $
    context "when run after tokensParser on + " $
      it
        "should return Right [Arith Add]"
        tokenizeAndParseAdd

-- loops
doLoopWithI :: Expectation
doLoopWithI =
  tokenizeAndParseTest "10 1 DO I . LOOP"
    `shouldBe` Right [Number 10, Number 1, DoLoop Loop {_loopbody = [Word "I", PrintCommand]}]

parseDoLoopWithI :: SpecWith ()
parseDoLoopWithI =
  describe "parseFromText" $
    context "when parsing a Do Loop with Index printing" $
      it
        "should parse it as a loop with Index and Print in the body"
        doLoopWithI

ifInsidedoLoop :: Expectation
ifInsidedoLoop =
  tokenizeAndParseTest "10 1 DO I 3 = IF .\"equal to 3\" THEN  LOOP"
    `shouldBe` Right
      [ Number 10,
        Number 1,
        DoLoop Loop {_loopbody = [Word "I", Number 3, Word "=", If [PrintStringLiteral "equal to 3"]]}
      ]

parseDoLoopWithIF :: SpecWith ()
parseDoLoopWithIF =
  describe "parseFromText" $
    context "when parsing a Do Loop with an If statement" $
      it
        "should parse it as a loop with the if in the body"
        ifInsidedoLoop

ifInsidepLoop :: Expectation
ifInsidepLoop =
  tokenizeAndParseTest "10 1 DO I 3 = IF .\"equal to 3\" THEN  2 +LOOP"
    `shouldBe` Right
      [ Number 10,
        Number 1,
        PlusLoop Loop {_loopbody = [Word "I", Number 3, Word "=", If [PrintStringLiteral "equal to 3"], Number 2]}
      ]

parsePLoopWithIF :: SpecWith ()
parsePLoopWithIF =
  describe "parseFromText" $
    context "when parsing a Plus Loop with an If statement" $
      it
        "should parse it as a loop with the if in the body"
        ifInsidepLoop

plusLoopWithI :: Expectation
plusLoopWithI =
  tokenizeAndParseTest "10 1 DO I . +LOOP"
    `shouldBe` Right
      [ Number 10,
        Number 1,
        PlusLoop Loop {_loopbody = [Word "I", PrintCommand]}
      ]

parsePLoopWithI :: SpecWith ()
parsePLoopWithI =
  describe "parseFromText" $
    context "when parsing a Plus Loop with Index printing" $
      it
        "should parse it as a plus loop with Index and Print in the body"
        plusLoopWithI

untilLoopWithAdd :: Expectation
untilLoopWithAdd =
  tokenizeAndParseTest "1 BEGIN 1 + DUP 10 = UNTIL"
    `shouldBe` Right
      [ Number 1,
        UntilLoop Loop {_loopbody = [Number 1, Word "+", Word "DUP", Number 10, Word "="]}
      ]

parseUntilLoop :: SpecWith ()
parseUntilLoop =
  describe "parseFromText" $
    context "when parsing an Until Loop with +" $
      it
        "should parse it as until loop with Add in the body"
        untilLoopWithAdd

untilLoopParseErr :: Expectation
untilLoopParseErr =
  tokenizeAndParseTest "1 Do 1 + DUP 10 = UNTIL"
    `shouldBe` Left ParseErr

parseErrUntilLoop :: SpecWith ()
parseErrUntilLoop =
  describe "parseFromText" $
    context "when parsing an Until Loop beginning with DO insted of UNTIL" $
      it
        "should throw a ParseErr"
        untilLoopParseErr

incompleteLoopSyntaxError :: Expectation
incompleteLoopSyntaxError =
  tokenizeAndParseTest "10 1 DO I ."
    `shouldBe` Left SyntaxError

parseUnclosedLoop :: SpecWith ()
parseUnclosedLoop =
  describe "parseFromText" $
    context "when parsing an unterminated Loop " $
      it
        "should throw a SyntaxError"
        incompleteLoopSyntaxError

-- Definitions
defNewWord :: Expectation
defNewWord =
  tokenizeAndParseTest " : anewword SWAP - ; "
    `shouldBe` Right [Def (ForthVal.Fun "anewword" [Word "SWAP", Word "-"])]

parseDefWord :: SpecWith ()
parseDefWord =
  describe "parseFromText" $
    context "when parsing a function definition" $
      it
        "should parse it as a function with definition in the body"
        defNewWord

unclosedDef :: Expectation
unclosedDef =
  tokenizeAndParseTest " : anewword SWAP -  "
    `shouldBe` Left SyntaxError

parseIncompleteDef :: SpecWith ()
parseIncompleteDef =
  describe "parseFromText" $
    context "when parsing an unterminated function definition" $
      it
        "should throw a SyntaxError"
        unclosedDef

singleSemicolon :: Expectation
singleSemicolon =
  tokenizeAndParseTest " anewword ; "
    `shouldBe` Left SyntaxError

parseSingleSemicolon :: SpecWith ()
parseSingleSemicolon =
  describe "parseFromText" $
    context "when trying to parse a semicolon without an opening colon" $
      it
        "should throw a SyntazError"
        singleSemicolon

ifInsideDef :: Expectation
ifInsideDef =
  tokenizeAndParseTest " : afunction 3 = IF .\"equal to 3\" THEN  ; "
    `shouldBe` Right
      [ Def
          ( ForthVal.Fun
              "afunction"
              [ Number 3,
                Word "=",
                If [PrintStringLiteral "equal to 3"]
              ]
          )
      ]

parseifInsideDef :: SpecWith ()
parseifInsideDef =
  describe "parseFromText" $
    context "when parsing an if statement in a function definition" $
      it
        "should parse it as a definition with the if in the body"
        ifInsideDef

recurseInsideDef :: Expectation
recurseInsideDef =
  tokenizeAndParseTest " : arecursivefunction DUP 3 < IF 1 + RECURSE THEN  ; "
    `shouldBe` Right
      [ Def
          ( ForthVal.Fun
              "arecursivefunction"
              [ Word "DUP",
                Number 3,
                Word "<",
                If [Number 1, Word "+", Recurse]
              ]
          )
      ]

parseRecInsideDef :: SpecWith ()
parseRecInsideDef =
  describe "parseFromText" $
    context "when parsing recursion in a function definition" $
      it
        "should parse it as a definition with the Recurse word in the body"
        recurseInsideDef

-- IF
ifvalsParsed :: Expectation
ifvalsParsed =
  tokenizeAndParseTest " IF 2 + THEN"
    `shouldBe` Right
      [ If
          [ Number 2,
            Word "+"
          ]
      ]

parseifVals :: SpecWith ()
parseifVals =
  describe "parseFromText" $
    context "when parsing if" $
      it
        "should have the body inside the if statement"
        ifvalsParsed

loopInsideIf :: Expectation
loopInsideIf =
  tokenizeAndParseTest " IF 10 1 DO I . LOOP THEN"
    `shouldBe` Right
      [ If
          [ Number 10,
            Number 1,
            DoLoop Loop {_loopbody = [Word "I", PrintCommand]}
          ]
      ]

parseLoopInsideIf :: SpecWith ()
parseLoopInsideIf =
  describe "parseFromText" $
    context "when parsing a do loop inside an if statement" $
      it
        "should have the loop inside the if statement"
        loopInsideIf

ifInsideIf :: Expectation
ifInsideIf =
  tokenizeAndParseTest " IF IF 10 1 DO I . LOOP THEN THEN"
    `shouldBe` Right
      [ If
          [ If
              [ Number 10,
                Number 1,
                DoLoop Loop {_loopbody = [Word "I", PrintCommand]}
              ]
          ]
      ]

parseIFInsideIf :: SpecWith ()
parseIFInsideIf =
  describe "parseFromText" $
    context "when parsing an if statement inside an if statement" $
      it
        "should have the second if inside the first if statement"
        ifInsideIf

ifInsideElse :: Expectation
ifInsideElse =
  tokenizeAndParseTest " IF 1 ELSE IF 10 1 DO I . LOOP THEN THEN"
    `shouldBe` Right
      [ IfElse
          [Number 1]
          [ If
              [ Number 10,
                Number 1,
                DoLoop Loop {_loopbody = [Word "I", PrintCommand]}
              ]
          ]
      ]

parseIFInsideElse :: SpecWith ()
parseIFInsideElse =
  describe "parseFromText" $
    context "when parsing an if statement inside an else block" $
      it
        "should have the second if inside the else block"
        ifInsideElse

ifElseInsideElse :: Expectation
ifElseInsideElse =
  tokenizeAndParseTest " IF 1 ELSE IF 10 1 DO I . LOOP ELSE 2 . THEN THEN"
    `shouldBe` Right
      [ IfElse
          [Number 1]
          [ IfElse
              [ Number 10,
                Number 1,
                DoLoop Loop {_loopbody = [Word "I", PrintCommand]}
              ]
              [Number 2, PrintCommand]
          ]
      ]

parseIFElseInsideElse :: SpecWith ()
parseIFElseInsideElse =
  describe "parseFromText" $
    context "when parsing an if else statement inside an else block" $
      it
        "should have the second if else inside the else block"
        ifElseInsideElse

-----------Tests for evaluation-------
--------------------------------------
-- defining new words
newWordInserted :: Expectation
newWordInserted =
  eval initialEnv (Def (ForthVal.Fun "new" [Arith Add, Arith Times]))
    `shouldSatisfy` env_contains_word "new"

evalNewWord :: SpecWith ()
evalNewWord =
  describe "eval" $
    context "when evaluating a definition" $
      it
        "should insert the new word"
        newWordInserted

definedWord :: Expectation
definedWord =
  stackState
    ( envWithNewWord >>= (`eval` Word "new")
    )
    `shouldBe` Right [12]

evalDefinitions :: SpecWith ()
evalDefinitions =
  describe "eval" $
    context "when defining a word and using it" $
      it
        "executes the new word"
        definedWord

unknownWordError :: Expectation
unknownWordError =
  eval initialEnv (Word "unkownWord")
    `shouldBe` Left UnknownWord

evalUndefinedError :: SpecWith ()
evalUndefinedError =
  describe "eval" $
    context "when trying to use an undefined word" $
      it
        "throws UnknownWord error"
        unknownWordError

-- arithmetic
multiplyResultOnStack :: Expectation
multiplyResultOnStack =
  stackState (eval envWithStackNumbers (Arith Times))
    `shouldBe` Right [2, 4]

evalMultiply :: SpecWith ()
evalMultiply =
  describe "eval" $
    context "when evaluating multiplication" $
      it
        "pushes result on top of the stack"
        multiplyResultOnStack

addResultOnStack :: Expectation
addResultOnStack =
  stackState (eval envWithStackNumbers (Arith Add))
    `shouldBe` Right [3, 4]

evalAdd :: SpecWith ()
evalAdd =
  describe "eval" $
    it "pushes result on top of the stack" $
      stackState (eval envWithStackNumbers (Arith Add))
        `shouldBe` Right [3, 4]

-- Stack manipulation
stackDrop :: Expectation
stackDrop =
  stackState (eval envWithStackNumbers (Manip Drop))
    `shouldBe` Right [2, 4]

evalDrop :: SpecWith ()
evalDrop =
  describe "eval" $
    context "when evaluating stack manipulation drop" $
      it
        "drops top of the stack"
        stackDrop

stackDup :: Expectation
stackDup =
  stackState (eval envWithStackNumbers (Manip Dup))
    `shouldBe` Right [1, 1, 2, 4]

evalDup :: SpecWith ()
evalDup =
  describe "eval" $
    context "when evaluating stack manipulation dup" $
      it
        "duplicates top element of the stack"
        stackDup

stackOver :: Expectation
stackOver =
  stackState (eval envWithStackNumbers (Manip Over))
    `shouldBe` Right [2, 1, 2, 4]

evalOver :: SpecWith ()
evalOver =
  describe "eval" $
    context "when evaluating stack manipulation over" $
      it
        "repeats top of the stack as 3rd element"
        stackOver

stackDupEmptyStack :: Expectation
stackDupEmptyStack =
  stackState (eval initialEnv (Manip Dup))
    `shouldBe` Left StackUnderflow

evalDupExeption :: SpecWith ()
evalDupExeption =
  describe "eval" $
    context "when evaluating stack manipulation dup on empty stack" $
      it
        "throws StackUnderFlow exception"
        stackDupEmptyStack

-- list of values
addressExecuted :: Expectation
addressExecuted =
  stackState (eval envWithStackNumbers (Address 0))
    `shouldBe` Right [3, 4]

evalExecutionToken :: SpecWith ()
evalExecutionToken =
  describe "eval" $
    context "when evaluating an execution token" $
      it
        "executes the definition at the address"
        addressExecuted

dictLookuplikeAddress :: Expectation
dictLookuplikeAddress =
  stackState (eval envWithStackNumbers (Address 0))
    `shouldBe` stackState (eval envWithStackTop0 DictLookup)

evalDictLookupEqAddress :: SpecWith ()
evalDictLookupEqAddress =
  describe "eval" $
    context "when evaluating function definition lookup" $
      it
        "executes the definition at the address"
        dictLookuplikeAddress

nameLookupPlus :: Expectation
nameLookupPlus =
  stackState (eval envWithStackNumbers (NameLookup "+"))
    `shouldBe` Right [0, 1, 2, 4]

evalNameLookup :: SpecWith ()
evalNameLookup =
  describe "eval" $
    context "when looking up execution token by name" $
      it
        "returns execution token on stack"
        nameLookupPlus

nameLookupUnknown :: Expectation
nameLookupUnknown =
  eval envWithStackNumbers (NameLookup "undefinedWord")
    `shouldBe` Left UnknownWord

evalNameLookupUnkownError :: SpecWith ()
evalNameLookupUnkownError =
  describe "eval" $
    context "when looking up execution token of non existing word" $
      it
        "throws UnkownWord error"
        nameLookupUnknown

forthvalsAllExecuted :: Expectation
forthvalsAllExecuted =
  stackState (eval envWithStackNumbers (Forthvals [Manip Drop, Manip Dup]))
    `shouldBe` Right [2, 2, 4]

evalManyForthExpressions :: SpecWith ()
evalManyForthExpressions =
  describe "eval" $
    context "when evaluating a list of forthvals" $
      it
        "executes all of them"
        forthvalsAllExecuted

-- if else
ifExecutedIfTrue :: Expectation
ifExecutedIfTrue =
  stackState (eval envWithStackNumbers (If [Manip Drop, Manip Dup]))
    `shouldBe` Right [4, 4]

evalIfExecutedIfTrue :: SpecWith ()
evalIfExecutedIfTrue =
  describe "eval" $
    context "when evaluating if" $
      it
        "executes if stack top is not 0"
        ifExecutedIfTrue

ifNotExecutedIfFalse :: Expectation
ifNotExecutedIfFalse =
  stackState (eval envWithStackTop0 (If [Manip Drop, Manip Dup]))
    `shouldBe` Right [1, 2, 4]

evalIFNotExecutedIfFalse :: SpecWith ()
evalIFNotExecutedIfFalse =
  describe "eval" $
    context "when evaluating if" $
      it
        "does not execute if stack top is 0"
        ifNotExecutedIfFalse

elseExecutedIfFalse :: Expectation
elseExecutedIfFalse =
  stackState
    ( eval
        envWithStackTop0
        ( IfElse
            [ Manip Drop,
              Manip Dup
            ]
            [Number 10]
        )
    )
    `shouldBe` Right [10, 1, 2, 4]

evalElseExecuted :: SpecWith ()
evalElseExecuted =
  describe "eval" $
    context "when evaluating if else" $
      it
        "executes else if stack top is 0"
        elseExecutedIfFalse

-- loops
doloopExecuted :: Expectation
doloopExecuted =
  stackState (eval envWithStackTop0 (DoLoop Loop {_loopbody = [Number 3]}))
    `shouldBe` Right [3, 2, 4]

evalDoLoopExecuted :: SpecWith ()
evalDoLoopExecuted =
  describe "eval" $
    context "when evaluating do loop" $
      it
        "loppbody is executed"
        doloopExecuted

doloopExecutedntimes :: Expectation
doloopExecutedntimes =
  stackState
    ( eval
        envWithStackTop0
        ( Forthvals
            [ Number 5,
              Number 0,
              DoLoop Loop {_loopbody = [Number 3]}
            ]
        )
    )
    `shouldBe` Right [3, 3, 3, 3, 3, 0, 1, 2, 4]

evalDooLoopnTimes :: SpecWith ()
evalDooLoopnTimes =
  describe "eval" $
    context "when evaluating do loop" $
      it
        "executes from start to finish index"
        doloopExecutedntimes

doloopnotExecuted :: Expectation
doloopnotExecuted =
  stackState
    ( eval
        envWithStackTop0
        ( Forthvals
            [ Number 0,
              DoLoop Loop {_loopbody = [Number 3]}
            ]
        )
    )
    `shouldBe` Right [1, 2, 4]

evalDoLoopNotExecuted :: SpecWith ()
evalDoLoopNotExecuted =
  describe "eval" $
    context "when evaluating do loop with start and finish index equal" $
      it
        "loopbody is not executed"
        doloopnotExecuted

indexPutOnStack :: Expectation
indexPutOnStack =
  stackState
    ( eval
        initialEnv
        ( Forthvals
            [ Number 5,
              Number 0,
              DoLoop Loop {_loopbody = [Word "I"]}
            ]
        )
    )
    `shouldBe` Right [4, 3, 2, 1, 0]

evalIndexInLoop :: SpecWith ()
evalIndexInLoop =
  describe "eval" $
    context "when using I in do loop " $
      it
        "puts the current index on the stack"
        indexPutOnStack

ploopExecuted :: Expectation
ploopExecuted =
  stackState
    ( eval
        envWithStackTop0
        ( PlusLoop
            Loop
              { _loopbody =
                  [ Number 4,
                    Number 1
                  ]
              }
        )
    )
    `shouldBe` Right [4, 2, 4]

evalPlusLoopExecuted :: SpecWith ()
evalPlusLoopExecuted =
  describe "eval" $
    context "when evaluating plus loop" $
      it
        "loopbody is executed"
        ploopExecuted

ploopIncreaseIndex :: Expectation
ploopIncreaseIndex =
  stackState
    ( eval
        envWithStackNumbers
        ( Forthvals
            [ Number 5,
              Number 2,
              PlusLoop Loop {_loopbody = [Number 3, Number 2]}
            ]
        )
    )
    `shouldBe` Right [3, 3, 1, 2, 4]

evalPlusLoopIncreasesIndex :: SpecWith ()
evalPlusLoopIncreasesIndex =
  describe "eval" $
    context "when evaluating plus loop" $
      it
        "increases the index by number on top of stack"
        ploopIncreaseIndex

untilloopRunUntilTrue :: Expectation
untilloopRunUntilTrue =
  stackState
    ( eval
        envWithStackNumbers
        ( UntilLoop
            Loop
              { _loopbody =
                  [ Number 2,
                    Arith Equal
                  ]
              }
        )
    )
    `shouldBe` Right [4]

evalUntilLoopUntilTrue :: SpecWith ()
evalUntilLoopUntilTrue =
  describe "eval" $
    context "when evaluating until loop" $
      it
        "runs until stack top is True (non-zero)"
        untilloopRunUntilTrue

untilloopStopOn2 :: Expectation
untilloopStopOn2 =
  stackState (eval envWithStackNumbers (UntilLoop Loop {_loopbody = [Manip Drop]}))
    `shouldBe` Right [4]

evalUntilLoopStopCondition :: SpecWith ()
evalUntilLoopStopCondition =
  describe "eval" $
    context "when evaluating until loop" $
      it
        "stops execution if stack top is 2"
        untilloopStopOn2

-- Variables
uninitializedVar :: Expectation
uninitializedVar =
  eval initialEnv (Word "myvar")
    `shouldBe` Left UnknownWord

evalUninitializedException :: SpecWith ()
evalUninitializedException =
  describe "eval" $
    context "when acessing uninitialized variable" $
      it
        "throws exception"
        uninitializedVar

varExistsAfterInitialization :: Expectation
varExistsAfterInitialization =
  stackTail
    ( eval
        initialEnv
        ( Forthvals
            [ Variable "myvar",
              Word "myvar"
            ]
        )
    )
    `shouldBe` Right []

evalInitializeVar :: SpecWith ()
evalInitializeVar =
  describe "eval" $
    context "when initializing a variable and retrieving it" $
      it
        "puts memory address of new variable on stack"
        varExistsAfterInitialization

variableCanBeAssignedAndRetrieved :: Expectation
variableCanBeAssignedAndRetrieved =
  stackTop
    ( eval
        initialEnv
        ( Forthvals
            [ Variable "myvar",
              Number 2,
              Word "myvar",
              Word "!",
              Word "myvar",
              Word "@"
            ]
        )
    )
    `shouldBe` Right 2

evalAssignVar :: SpecWith ()
evalAssignVar =
  describe "eval" $
    context "when assigning a variable and retrieving it" $
      it
        "puts value assigned to variable on stack"
        variableCanBeAssignedAndRetrieved

-- Memory operations
numCanBeStoredAndRetrieved :: Expectation
numCanBeStoredAndRetrieved =
  stackTop
    ( eval
        initialEnv
        ( Forthvals
            [ Variable "myvar",
              Number 2,
              Word "myvar",
              Mem Store,
              Word "myvar",
              Mem Retrieve
            ]
        )
    )
    `shouldBe` Right 2

evalStoreRetrieve :: SpecWith ()
evalStoreRetrieve =
  describe "eval" $
    context "when evaluating memory storage and retrieval" $
      it
        "does the same as the ! and @ words"
        numCanBeStoredAndRetrieved

cellAllotInitializesMemory :: Expectation
cellAllotInitializesMemory =
  stackState
    ( eval envWithStackNumbers (Mem Allot)
        >>= ( `eval`
                Forthvals
                  [ Number 2,
                    Mem Retrieve
                  ]
            )
    )
    `shouldBe` Right [0, 4]

evalCellAllot :: SpecWith ()
evalCellAllot =
  describe "eval" $
    context "when evaluating cell allot" $
      it
        "allocates memory"
        cellAllotInitializesMemory

commaStoreLeavesNewAddressOnStack :: Expectation
commaStoreLeavesNewAddressOnStack = stackTop (eval envWithStackNumbers (Mem CommaStore)) `shouldBe` Right 3

evalCommaStore :: SpecWith ()
evalCommaStore =
  describe "eval" $
    context "when evaluating comma" $
      it
        "leaves the next memory address on the stack"
        commaStoreLeavesNewAddressOnStack

commaStoreDump :: Expectation
commaStoreDump =
  stackState
    ( eval
        envWithStackNumbers
        ( Forthvals
            [ Mem CommaStore,
              Number 5,
              Mem CommaStore,
              Number 2,
              Number 2,
              Word "DUMP"
            ]
        )
    )
    `shouldBe` Right [4, 4]

evalCommaStoreDump :: SpecWith ()
evalCommaStoreDump =
  describe "eval" $
    context "when evaluating DUMP" $
      it
        "takes 2 numbers off the stack"
        commaStoreDump

commaStoreTwice :: Expectation
commaStoreTwice =
  stackState
    ( eval
        envWithStackNumbers
        ( Forthvals
            [ Mem CommaStore,
              Number 5,
              Mem CommaStore
            ]
        )
    )
    `shouldBe` Right [4, 4]

evalCommaStoreTwice :: SpecWith ()
evalCommaStoreTwice =
  describe "eval" $
    context "when evaluating comma twice" $
      it
        "leaves the next memory address on the stack"
        commaStoreTwice

accessingUninitializedMemoryError :: Expectation
accessingUninitializedMemoryError =
  eval envWithStackNumbers (Mem Retrieve)
    `shouldBe` Left MemoryAccessError

errorOnAccessingUninitializedMemory :: SpecWith ()
errorOnAccessingUninitializedMemory =
  describe "eval"
    $ context
      "when accessing uninitialized and unwritten memory"
    $ it
      "throws MemoryAccessError"
      accessingUninitializedMemoryError

accessingMemoryWithoutAddressError :: Expectation
accessingMemoryWithoutAddressError =
  eval initialEnv (Mem Retrieve)
    `shouldBe` Left StackUnderflow

errorOnAccessingMemoryWithoutAddress :: SpecWith ()
errorOnAccessingMemoryWithoutAddress =
  describe "eval"
    $ context
      "when accessing memory with an empty stack"
    $ it
      "throws StackUnderFlowError"
      accessingMemoryWithoutAddressError

dumpUninitializedMemoryError :: Expectation
dumpUninitializedMemoryError =
  eval envWithStackNumbers (Word "DUMP")
    `shouldBe` Left MemoryAccessError

errorOnDumpUninitializedMemory :: SpecWith ()
errorOnDumpUninitializedMemory =
  describe "eval"
    $ context
      "when using DUMP word on uninitialized memory"
    $ it
      "throws MemoryAccessError"
      dumpUninitializedMemoryError

-- Environment
-------------
environmentInitialDef :: SpecWith ()
environmentInitialDef = describe "initialDef" $ do
  it "should contain +" initialDefs_addition_operator

-- printing
-------------
printCommandRemovesFromStack :: Expectation
printCommandRemovesFromStack =
  stackState (eval envWithStackNumbers PrintCommand)
    `shouldBe` Right [2, 4]

evalPrintRemovedFromStack :: SpecWith ()
evalPrintRemovedFromStack =
  describe "eval" $
    context "when evaluating a print command" $
      it
        "removes the top element from the stack"
        printCommandRemovesFromStack

printCommandAppendsToPrintStr :: Expectation
printCommandAppendsToPrintStr =
  printStrState (eval envWithStackNumbers PrintCommand)
    `shouldBe` Right ["1"]

evalPrintAppendedToPrintStr :: SpecWith ()
evalPrintAppendedToPrintStr =
  describe "eval" $
    context "when evaluating a print command" $
      it
        "Appends the top element of the stack to printStr"
        printCommandAppendsToPrintStr

printStrLitAppendsToPrintStr :: Expectation
printStrLitAppendsToPrintStr = printStrState (eval envWithStackNumbers (PrintStringLiteral "string")) `shouldBe` Right ["string"]

evalPrintStringLitAppendedToPrintStr :: SpecWith ()
evalPrintStringLitAppendedToPrintStr =
  describe "eval" $
    context "when evaluating a print string literal command" $
      it
        "Appends the string literal to printStr"
        printStrLitAppendsToPrintStr

-- ASCII
ascii0 :: Expectation
ascii0 = printStrState (eval envWithStackTop0 Ascii) `shouldBe` Left NonAsciiCode

evalNonAscii :: SpecWith ()
evalNonAscii =
  describe "eval" $
    context "when evaluating an ascii code " $
      it
        "Appends the ascii character to printStr"
        ascii0

asciiA :: Expectation
asciiA = printStrState (eval envWithStackTop65 Ascii) `shouldBe` Right ["A"]

evalAsciiAppendedToPrintStr :: SpecWith ()
evalAsciiAppendedToPrintStr =
  describe "eval" $
    context "when evaluating an ascii code " $
      it
        "Appends the ascii character to printStr"
        asciiA

asciiCodeA :: Expectation
asciiCodeA = stackState (eval initialEnv (Key 'A')) `shouldBe` Right [65]

evalAsciiCodeAOnStack :: SpecWith ()
evalAsciiCodeAOnStack =
  describe "eval" $
    context "when evaluating an ascii Char A " $
      it
        "pushes the ascii code to the stack"
        asciiCodeA

-- storing and typing strings
storeAndTypeString :: Expectation
storeAndTypeString =
  printStrState (envWithStringInMem >>= typeStringMem)
    `shouldBe` Right ["a String"]

evalStoreAndTypeString :: SpecWith ()
evalStoreAndTypeString =
  describe "eval" $
    context "when storing a string in memory and then typing it" $
      it
        "pushes the string to the print stack"
        storeAndTypeString

-- Recursion
recurseIF :: Expectation
recurseIF =
  stackState recurse
    `shouldBe` Right [4]

recurseIfTrue :: SpecWith ()
recurseIfTrue =
  describe "eval" $
    context "when evaluating recursion inside If" $
      it
        "recurses while True"
        recurseIF

-- evaluate source file
fileDoesNotExist :: Expectation
fileDoesNotExist =
  runExceptT (evalT initialEnv (SourceFile "nonExistentFile"))
    `shouldReturn` Left (FileNotFound "nonExistentFile")

evalSourceDoesNotExist :: SpecWith ()
evalSourceDoesNotExist =
  describe "evalT" $
    context "when evaluating a file that does not exits" $
      it
        "throws FileNotFound error"
        fileDoesNotExist

sourceFile :: Expectation
sourceFile =
  runExceptT (evalT initialEnv (SourceFile "test/testfile.forth"))
    `shouldReturn` Right initialEnv

evalSource :: SpecWith ()
evalSource =
  describe "evalT" $
    context "when evaluating a test source file " $
      it
        "reverts to previous state after evaluation"
        sourceFile

-- evalT with non- IO value
evalTNoIO :: Expectation
evalTNoIO =
  runExceptT (evalT initialEnv (Number 0))
    `shouldReturn` Right initialEnv {_stack = [0]}

evalTwithPureValue :: SpecWith ()
evalTwithPureValue =
  describe "evalT" $
    context "when evaluated on a pure value" $
      it
        "wraps the result in ExceptT"
        evalTNoIO

--------------------------------------
-- Transpiler Tests
---------------------------------------
subtractAfterAdd :: Expectation
subtractAfterAdd =
  parseTranspileGenerateOutputFromText " 1 2 + 3 -"
    `shouldBe` "(1 + 2) - 3\n"

transpileAddAndSubtract :: SpecWith ()
transpileAddAndSubtract =
  describe "parseTranspileOutputFromText" $
    context "when transpiling an arithmetic expression" $
      it
        "respects the order of operations"
        subtractAfterAdd

modOrderCorrect :: Expectation
modOrderCorrect =
  parseTranspileGenerateOutputFromText "3 2 MOD 1 ="
    `shouldBe` "(3 % 2) == 1\n"

transpileMod :: SpecWith ()
transpileMod =
  describe "parseTranspileOutputFromText" $
    context "when transpiling MOD" $
      it
        "outputs correct order"
        modOrderCorrect

printExpression :: Expectation
printExpression =
  parseTranspileGenerateOutputFromText " 1 2 + 3 - . "
    `shouldBe` "print((1 + 2) - 3)\n"

transpilePrintExpression :: SpecWith ()
transpilePrintExpression =
  describe "parseTranspileOutputFromText" $
    context "when transpiling and a print statement of an arithmetic expression" $
      it
        "has the print statement around the expression"
        printExpression

printStringLiteral :: Expectation
printStringLiteral =
  parseTranspileGenerateOutputFromText " .\"hello world\""
    `shouldBe` "print(\"hello world\")\n"

transpilePrintStringLiteral :: SpecWith ()
transpilePrintStringLiteral =
  describe "parseTranspileOutputFromText" $
    context "when transpiling a print statement of a string literal" $
      it
        "has the print statement around the string"
        printStringLiteral

ifGreater :: Expectation
ifGreater = parseTranspileGenerateOutputFromText "3 2 > IF .\"hello world\" THEN" `shouldBe` "if 3 > 2:\n    print(\"hello world\")\n"

transpileifGreater :: SpecWith ()
transpileifGreater =
  describe "parseTranspileOutputFromText" $
    context "when transpiling if greater statements" $
      it
        "indents the blocks correctly"
        ifGreater

nestedIf :: Expectation
nestedIf =
  parseTranspileGenerateOutputFromText "2 2 = IF 3 1 > IF 5 .THEN THEN"
    `shouldBe` "if 2 == 2:\n    if 3 > 1:\n        print(5)\n"

transpilenestedIf :: SpecWith ()
transpilenestedIf =
  describe "parseTranspileOutputFromText" $
    context "when transpiling nested if statements" $
      it
        "indents the blocks correctly"
        nestedIf

nestedIfElse :: Expectation
nestedIfElse =
  parseTranspileGenerateOutputFromText "2 2 = IF 3 1 > IF 5 . ELSE 3 . THEN ELSE 1 . THEN"
    `shouldBe` "if 2 == 2:\n    if 3 > 1:\n        print(5)\n    else:\n        print(3)\nelse:\n    print(1)\n"

transpilenestedIfElse :: SpecWith ()
transpilenestedIfElse =
  describe "parseTranspileOutputFromText" $
    context "when transpiling nested if else statements" $
      it
        "indents the blocks correctly"
        nestedIfElse

-----------Helper functions------------
---------------------------------------

envWithStringInMem :: Either ForthErr Env
envWithStringInMem = eval initialEnv (StoreString "a String")

typeStringMem :: Env -> Either ForthErr Env
typeStringMem env = eval env Type

env_contains_word :: T.Text -> Either ForthErr Env -> Bool
env_contains_word _ (Left _) = False
env_contains_word text (Right env) = Map.member text (_names env)

envWithStackNumbers :: Env
envWithStackNumbers = Env initialNames initialDefs [1, 2, 4] (IM.singleton 0 0) 1 []

envWithStackTop0 :: Env
envWithStackTop0 = Env initialNames initialDefs [0, 1, 2, 4] (IM.singleton 0 0) 1 []

envWithStackTop65 :: Env
envWithStackTop65 = Env initialNames initialDefs [65, 1, 2, 4] (IM.singleton 0 0) 1 []

stackState :: Either ForthErr Env -> Either ForthErr [Int]
stackState = fmap _stack

stackTop :: Either ForthErr Env -> Either ForthErr Int
stackTop = fmap L.head . stackState

printStrState :: Either ForthErr Env -> Either ForthErr [String]
printStrState = fmap _printStr

stackTail :: Either ForthErr Env -> Either ForthErr [Int]
stackTail = fmap L.tail . stackState

envWithNewWord :: Either ForthErr Env
envWithNewWord = eval envWithStackNumbers (Def (ForthVal.Fun "new" [Arith Add, Arith Times]))

envForTestFile :: Either ForthErr Env
envForTestFile = eval (initialEnv {_stack = [1]}) (Def (ForthVal.Fun "anewword" [Manip Swap, Arith Sub]))

envWithRecursiveFunction :: Either ForthErr Env
envWithRecursiveFunction = eval envWithStackNumbers (Def (ForthVal.Fun "rec" [If [Number 2, Arith Less, Recurse]]))

recurse :: Either ForthErr Env
recurse = envWithRecursiveFunction >>= (`eval` Word "rec")

tokenizeAndParseTest :: Text -> Either ForthErr [ForthVal]
tokenizeAndParseTest = parseFromText "test"
