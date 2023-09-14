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

main :: IO ()
main =
  hspec $ do
    -- Tests for environment initial setup
    -----------------------------------------
    propInitialStack

    environmentInitialDef
    -- Parsing Tests
    ----------

    parseIdentifier
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
    errorOnAccessingUninitializedMemory
    errorOnAccessingMemoryWithoutAddress
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

-----------Tests for parsing----------
--------------------------------------
parseIdExists :: Expectation
parseIdExists = parse ideToken "file" "aword" `shouldBe` Right (Ide "aword")

parseIdentifier :: SpecWith ()
parseIdentifier =
  describe "ideToken" $
    it
      "should return an identifier Token"
      parseIdExists

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
unknownWordError = eval initialEnv (Word "unkownWord") `shouldBe` Left UnknownWord

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
    `shouldBe` Right [1, 2, 1, 4]

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
  stackState (eval envWithStackTop0 (IfElse [Manip Drop, Manip Dup] [Number 10]))
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
  stackState (eval envWithStackTop0 (Forthvals [Number 5, Number 0, DoLoop Loop {_loopbody = [Number 3]}]))
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
  stackState (eval envWithStackTop0 (Forthvals [Number 0, DoLoop Loop {_loopbody = [Number 3]}]))
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
  stackState (eval initialEnv (Forthvals [Number 5, Number 0, DoLoop Loop {_loopbody = [Word "I"]}]))
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
  stackState (eval envWithStackTop0 (PlusLoop Loop {_loopbody = [Number 1]}))
    `shouldBe` Right [1, 2, 4]

evalPlusLoopExecuted :: SpecWith ()
evalPlusLoopExecuted =
  describe "eval" $
    context "when evaluating plus loop" $
      it
        "loopbody is executed"
        ploopExecuted

ploopIncreaseIndex :: Expectation
ploopIncreaseIndex =
  stackState (eval envWithStackNumbers (Forthvals [Number 5, Number 2, PlusLoop Loop {_loopbody = [Number 2]}]))
    `shouldBe` Right [2, 2, 1, 2, 4]

evalPlusLoopIncreasesIndex :: SpecWith ()
evalPlusLoopIncreasesIndex =
  describe "eval" $
    context "when evaluating plus loop" $
      it
        "increases the index by number on top of stack"
        ploopIncreaseIndex

untilloopRunUntilTrue :: Expectation
untilloopRunUntilTrue =
  stackState (eval envWithStackNumbers (UntilLoop Loop {_loopbody = [Number 2, Arith Equal]}))
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
uninitializedVar = eval initialEnv (Word "myvar") `shouldBe` Left UnknownWord

evalUninitializedException :: SpecWith ()
evalUninitializedException =
  describe "eval" $
    context "when acessing uninitialized variable" $
      it
        "throws exception"
        uninitializedVar

varExistsAfterInitialization :: Expectation
varExistsAfterInitialization = stackTail (eval initialEnv (Forthvals [Variable "myvar", Word "myvar"])) `shouldBe` Right []

evalInitializeVar :: SpecWith ()
evalInitializeVar =
  describe "eval" $
    context "when initializing a variable and retrieving it" $
      it
        "puts memory address of new variable on stack"
        varExistsAfterInitialization

variableCanBeAssignedAndRetrieved :: Expectation
variableCanBeAssignedAndRetrieved = stackTop (eval initialEnv (Forthvals [Variable "myvar", Number 2, Word "myvar", Word "!", Word "myvar", Word "@"])) `shouldBe` Right 2

evalAssignVar :: SpecWith ()
evalAssignVar =
  describe "eval" $
    context "when assigning a variable and retrieving it" $
      it
        "puts value assigned to variable on stack"
        variableCanBeAssignedAndRetrieved

-- Memory operations
numCanBeStoredAndRetrieved :: Expectation
numCanBeStoredAndRetrieved = stackTop (eval initialEnv (Forthvals [Variable "myvar", Number 2, Word "myvar", Mem Store, Word "myvar", Mem Retrieve])) `shouldBe` Right 2

evalStoreRetrieve :: SpecWith ()
evalStoreRetrieve =
  describe "eval" $
    context "when evaluating memory storage and retrieval" $
      it
        "does the same as the ! and @ words"
        numCanBeStoredAndRetrieved

cellAllotInitializesMemory :: Expectation
cellAllotInitializesMemory = stackState ((eval envWithStackNumbers (Mem Allot)) >>= (`eval` (Forthvals [Number 2, Mem Retrieve]))) `shouldBe` Right [0, 4]

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

accessingUninitializedMemoryError :: Expectation
accessingUninitializedMemoryError = eval envWithStackNumbers (Mem Retrieve) `shouldBe` Left MemoryAccessError

errorOnAccessingUninitializedMemory :: SpecWith ()
errorOnAccessingUninitializedMemory =
  describe "eval"
    $ context
      "when accessing uninitialized and unwritten memory"
    $ it
      "throws MemoryAccessError"
      accessingUninitializedMemoryError

accessingMemoryWithoutAddressError :: Expectation
accessingMemoryWithoutAddressError = eval initialEnv (Mem Retrieve) `shouldBe` Left StackUnderflow

errorOnAccessingMemoryWithoutAddress :: SpecWith ()
errorOnAccessingMemoryWithoutAddress =
  describe "eval"
    $ context
      "when accessing memory with an empty stack"
    $ it
      "throws StackUnderFlowError"
      accessingMemoryWithoutAddressError

-- Environment
-------------
environmentInitialDef :: SpecWith ()
environmentInitialDef = describe "initialDef" $ do
  it "should contain +" initialDefs_addition_operator

-- printing
-------------
printCommandRemovesFromStack :: Expectation
printCommandRemovesFromStack = stackState (eval envWithStackNumbers PrintCommand) `shouldBe` Right [2, 4]

evalPrintRemovedFromStack :: SpecWith ()
evalPrintRemovedFromStack =
  describe "eval" $
    context "when evaluating a print command" $
      it
        "removes the top element from the stack"
        printCommandRemovesFromStack

printCommandAppendsToPrintStr :: Expectation
printCommandAppendsToPrintStr = printStrState (eval envWithStackNumbers PrintCommand) `shouldBe` Right ["1"]

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
storeAndTypeString = printStrState (envWithStringInMem >>= typeStringMem) `shouldBe` Right ["a String"]

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
  runExceptT (evalT initialEnv (SourceFile "nonExistentFile")) `shouldReturn` Left (FileNotFound "nonExistentFile")

evalSourceDoesNotExist :: SpecWith ()
evalSourceDoesNotExist =
  describe "evalT" $
    context "when evaluating a file that does not exits" $
      it
        "throws FileNotFound error"
        fileDoesNotExist

sourceFile :: Expectation
sourceFile =
  runExceptT (evalT initialEnv (SourceFile "test/testfile.forth")) `shouldReturn` Right initialEnv
evalSource :: SpecWith ()
evalSource =
  describe "evalT" $
    context "when evaluating a test source file " $
      it
        "reverts to previous state after evaluation"
        sourceFile

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
