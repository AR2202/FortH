{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Eval
  ( initialDefs,
    initialEnv,
    initialNames,
    printF,
    printStack,
    eval,
    evalT,
    evalAndPrintStackTop,
    evalInput,
    evalAndPrintStackTopRepl,
    evalInputRepl,
    evalFile,
    evalAndPrintDefDict,
    evalAndPrintMemory,
    evalAndPrintNames,
    printDefs,
    printMem,
    printNames,
  )
where

import ASCII
import Control.Exception (AsyncException (StackOverflow), catchJust)
import Control.Lens
import Control.Monad.Except (ExceptT (..), catchError, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.IntMap as IM
import Data.List as L
import qualified Data.Map as Map
import Data.Text as T
import Foreign.C (Errno (Errno))
import ForthVal
import Parser
import System.IO.Error (ioeGetErrorType, isDoesNotExistErrorType)
import Test.Hspec.Formatters (FailureRecord (failureRecordPath))

initialNames :: Names
initialNames =
  Map.fromList $
    Prelude.zip
      [ "+",
        "-",
        "*",
        "/",
        "=",
        "<",
        ">",
        "DUP",
        "DROP",
        "SWAP",
        "OVER",
        "ROT",
        "INVERT",
        "THEN",
        "!",
        "@",
        "ALLOT",
        "OR",
        "AND",
        "XOR",
        "NOT",
        "MOD",
        "I",
        "EXECUTE",
        "EMIT",
        "FILE-POSITION",
        "CELL",
        "DUMP",
        "NUMBER"
      ]
      [0 ..]

initialDefs :: Defs
initialDefs =
  IM.fromList $
    Prelude.zip
      [0 ..]
      [ Arith Add,
        Arith Sub,
        Arith Times,
        Arith Div,
        Arith Equal,
        Arith Less,
        Arith Greater,
        Manip Dup,
        Manip Drop,
        Manip Swap,
        Manip Over,
        Manip Rot,
        Manip Invert,
        Manip Drop,
        Mem Store,
        Mem Retrieve,
        Mem Allot,
        Arith Or,
        Arith And,
        Arith Xor,
        Arith Not,
        Arith Mod,
        Forthvals [Number 0, Mem Retrieve],
        DictLookup,
        Ascii,
        Forthvals [Mem Retrieve, Number 0],
        Forthvals [Number 1, Mem Cellsize],
        Forthvals [Mem Cellsize, Manip Over, Arith Add, Manip Swap, PlusLoop (Loop [PrintStringLiteral "\n", Word "I", Mem Retrieve, PrintCommand, Number 1, Mem Cellsize])],
        FromStr
      ]

initialEnv :: Env
initialEnv = Env initialNames initialDefs [] (IM.singleton 0 0) 1 []

printF :: Env -> IO ()
printF env =
  case _stack env of
    [] -> return ()
    (x : xs) -> print x

printStack :: Env -> IO ()
printStack env = mapM_ print $ L.reverse $ env ^. stack

printDefs :: Env -> IO ()
printDefs env = print $ env ^. definitions

printNames :: Env -> IO ()
printNames env = print $ env ^. names

printMem :: Env -> IO ()
printMem env = print $ env ^. mem

eval :: Env -> ForthVal -> Either ForthErr Env
eval env (Number x) = evalNum env x
eval env (Arith Not) = evalNot env
eval env (Arith op) = evalOp env op
eval env (Manip Dup) = evalDup env
eval env (Manip Drop) = evalDrop env
eval env (Manip Swap) = evalSwap env
eval env (Manip Over) = evalOver env
eval env (Manip Rot) = evalRot env
eval env (Manip Invert) = evalInvert env
eval env (NameLookup name) = evalNameLookup env name
eval env (Word name) = evalWord env name
eval env (Address i) = evalAddress env i
eval env (Def fun) = evalDef env fun
eval env (Forthvals forthvals) = evalForthvals env forthvals
eval env (If forthvals) = evalIf env forthvals
eval env (IfElse ifvals elsevals) = evalIfElse env ifvals elsevals
eval env (DoLoop loop) = evalDoLoop env loop
eval env (PlusLoop loop) = evalPlusLoop env loop
eval env (UntilLoop loop) = evalUntilLoop env loop
eval env (Variable varname) = evalVar env varname
eval env (Mem StoreNext) = evalMemStoreNext env
eval env (Mem Store) = evalMemStore env
eval env (Mem Retrieve) = evalMemRetrieve env
eval env (Mem Allot) = evalMemAllot env
eval env (Mem Cellsize) = evalMemCellsize env
eval env (Mem CommaStore) = evalMemComma env
eval env PrintCommand = evalPrint env
eval env (PrintStringLiteral t) = evalPrintString env t
eval env DictLookup = evalLookup env
eval env Ascii = evalAscii env
eval env (Key c) = evalKey env c
eval env Type = evalType env
eval env (StoreString s) = evalStoreStr env s
eval env FromStr = evalFromStr env
eval _ _ = Left ParseErr

evalFromStr :: Env -> Either ForthErr Env
evalFromStr env = dropfromPrintStack <$> (pushToStack <$> converted <*> evalType env)
  where
    converted = read . L.head . _printStr <$> evalType env

evalT :: Env -> ForthVal -> ExceptT ForthErr IO Env
evalT env (SourceFile f) = ExceptT $ catchJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing) (evalFile f >> return (Right env)) (\_ -> return (Left (FileNotFound f)))
evalT env ReadFile = evalReadFile env
evalT env val = ExceptT $ return $ eval env val

evalReadFile :: Env -> ExceptT ForthErr IO Env
evalReadFile env = ExceptT $ catchJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing) ((evalReadFile2String . evalFileId) env) (\_ -> return (Left (FileNotFound "no such file")))

evalReadFile2String :: Either ForthErr Env -> IO (Either ForthErr Env)
evalReadFile2String (Right env) = do
  contents <- readFile (L.head $ env ^. printStr)
  case _stack env of
    [] -> return (Left StackUnderflow)
    (x : xs) -> do
      print x
      return $ evalStoreStr (set stack xs $ over printStr L.tail env) (L.take x contents)
evalReadFile2String e = return e

evalFileId :: Env -> Either ForthErr Env
evalFileId env = evalNum env (env ^. memorycell) >>= flip evalOp Add >>= evalDup >>= flip evalNum (env ^. memorycell) >>= flip evalOp Add >>= evalSwap >>= evalMemRetrieve >>= evalType

evalStoreStr :: Env -> String -> Either ForthErr Env
evalStoreStr env s =
  case traverse ascii2num s of
    Nothing -> Left NonAsciiCode
    Just ns ->
      Right $
        env
          { _stack = L.length s : nextmemaddr (env ^. mem) : env ^. stack,
            _mem = L.foldl' (\mem' n -> IM.insert (nextmemaddr mem') n mem') (env ^. mem) ns
          }
  where
    nextmemaddr mem' = IM.size mem' * env ^. memorycell

evalType :: Env -> Either ForthErr Env
evalType env = case _stack env of
  [] -> Left StackUnderflow
  (x : xs) -> fetchStr x (over stack L.tail env) ""

fetchStr :: (Eq t, Num t) => t -> Env -> [Char] -> Either ForthErr Env
fetchStr 0 env s = Right $ dropStackTop $ pushToPrintStack (L.reverse s) env
fetchStr n env s = case evalDup env >>= evalMemRetrieve of
  Left err -> Left err
  Right env' -> case _stack env' of
    [] -> Left StackUnderflow
    [x] -> Left StackUnderflow
    (x : y : xs) -> case num2ascii x of
      Nothing -> Left NonAsciiCode
      Just c -> fetchStr (n - 1) (env' {_stack = y + env ^. memorycell : xs}) (c : s)

evalKey :: Env -> Char -> Either ForthErr Env
evalKey env c = case ascii2num c of
  Nothing -> Left NonAsciiCode
  Just n -> Right $ over stack (n :) env

evalAscii :: Env -> Either ForthErr Env
evalAscii env = case _stack env of
  [] -> Left StackUnderflow
  (x : xs) -> case num2ascii x of
    Nothing -> Left NonAsciiCode
    Just c -> Right $ set stack xs $ over printStr (return c :) env

evalNum :: Env -> Int -> Either ForthErr Env
evalNum env x = Right $ over stack (x :) env

evalNot :: Env -> Either ForthErr Env
evalNot env =
  case _stack env of
    [] -> Left StackUnderflow
    0 : xs -> Right $ over stack (1 :) env
    x : xs -> Right $ over stack (0 :) env

evalOp :: Env -> Operator -> Either ForthErr Env
evalOp env op =
  case _stack env of
    [] -> Left StackUnderflow
    [x] -> Left StackUnderflow
    x : y : zs -> Right $ set stack (operation op y x : zs) env

operation :: Operator -> (Int -> Int -> Int)
operation Add = (+)
operation Sub = (-)
operation Times = (*)
operation Div = div
operation Equal =
  \x y ->
    if x == y
      then 1
      else 0
operation Less =
  \x y ->
    if x < y
      then 1
      else 0
operation Greater =
  \x y ->
    if x > y
      then 1
      else 0
operation Or = orOp
operation And = andOp
operation Xor = xorOp
operation Mod = mod

andOp :: Int -> Int -> Int
andOp 0 _ = 0
andOp _ 0 = 0
andOp x y = 1

orOp :: Int -> Int -> Int
orOp 0 0 = 0
orOp _ _ = 1

xorOp :: Int -> Int -> Int
xorOp 0 0 = 0
xorOp 0 _ = 1
xorOp _ 0 = 1
xorOp _ _ = 0

evalDup :: Env -> Either ForthErr Env
evalDup env =
  case _stack env of
    [] -> Left StackUnderflow
    (x : xs) -> Right $ over stack (x :) env

evalDrop :: Env -> Either ForthErr Env
evalDrop env =
  case _stack env of
    [] -> Left StackUnderflow
    (x : xs) -> Right $ set stack xs env

evalSwap :: Env -> Either ForthErr Env
evalSwap env =
  case _stack env of
    [] -> Left StackUnderflow
    [x] -> Left StackUnderflow
    x : y : zs -> Right $ env {_stack = y : x : zs}

evalOver :: Env -> Either ForthErr Env
evalOver env =
  case _stack env of
    [] -> Left StackUnderflow
    [x] -> Left StackUnderflow
    x : y : zs -> Right $ env {_stack = y : x : y : zs}

evalRot :: Env -> Either ForthErr Env
evalRot env =
  case _stack env of
    [] -> Left StackUnderflow
    [x] -> Left StackUnderflow
    x : [y] -> Left StackUnderflow
    x : y : z : zs -> Right $ env {_stack = y : z : x : zs}

evalInvert :: Env -> Either ForthErr Env
evalInvert env =
  case _stack env of
    [] -> Left StackUnderflow
    0 : xs -> Right $ invertStackTop env
    x : xs -> Right $ invertStackTop env

evalPrint :: Env -> Either ForthErr Env
evalPrint env =
  case _stack env of
    [] -> Left StackUnderflow
    (x : xs) -> Right $ set stack xs $ over printStr (show x :) env

evalPrintString :: Env -> T.Text -> Either ForthErr Env
evalPrintString env t = Right $ over printStr (T.unpack t :) env

evalWord :: Env -> T.Text -> Either ForthErr Env
evalWord env name =
  case Map.lookup name (env ^. names) of
    Nothing -> Left UnknownWord
    Just i ->
      case IM.lookup i (env ^. definitions) of
        Nothing -> Left UnknownWord
        Just forthval -> eval env forthval

evalAddress :: Env -> Int -> Either ForthErr Env
evalAddress env i =
  case IM.lookup i (env ^. definitions) of
    Nothing -> Left UnknownWord
    Just forthval -> eval env forthval

evalLookup :: Env -> Either ForthErr Env
evalLookup env =
  case _stack env of
    [] -> Left StackUnderflow
    (x : xs) -> eval (set stack xs env) (Address x)

evalNameLookup :: Env -> T.Text -> Either ForthErr Env
evalNameLookup env name =
  case Map.lookup name (env ^. names) of
    Nothing -> Left UnknownWord
    Just i -> Right $ over stack (i :) env

evalDef :: Env -> Fun -> Either ForthErr Env
evalDef env fun =
  case evalDefBody (over stack (Map.size (env ^. names) + 1 :) env) (fun ^. body) of
    Nothing -> Left UnknownWord
    Just forthvals ->
      Right $
        env
          { _names = Map.insert newword newaddress (env ^. names),
            _definitions =
              IM.insert newaddress (Forthvals forthvals) (env ^. definitions)
          }
      where
        newword = fun ^. name
        newaddress = Map.size (env ^. names) + 1

evalForthvals :: Foldable t => Env -> t ForthVal -> Either ForthErr Env
evalForthvals env forthvals = L.foldl' eval' (Right env) forthvals
  where
    eval' eEnv forthval =
      case eEnv of
        Left error -> Left error
        Right env' -> eval env' forthval

evalIf :: Env -> [ForthVal] -> Either ForthErr Env
evalIf env forthvals =
  case _stack env of
    [] -> Left StackUnderflow
    0 : xs -> Right (dropStackTop env)
    _ -> eval (dropStackTop env) (Forthvals forthvals)

evalIfElse :: Env -> [ForthVal] -> [ForthVal] -> Either ForthErr Env
evalIfElse env ifvals elsevals =
  case _stack env of
    [] -> Left StackUnderflow
    0 : xs -> eval (dropStackTop env) (Forthvals elsevals)
    _ -> eval (dropStackTop env) (Forthvals ifvals)

evalDoLoop :: Env -> Loop -> Either ForthErr Env
evalDoLoop env loop =
  case _stack env of
    [] -> Left StackUnderflow
    [x] -> Left StackUnderflow
    (x : y : zs) -> Right (over mem (IM.insert 0 x) ((dropStackTop . dropStackTop) env)) >>= go x y (loop ^. loopbody)
  where
    go index stop forthvals env'
      | index >= stop = Right env'
      | otherwise = eval (save2Mem 0 index env') (Forthvals forthvals) >>= go (index + 1) stop forthvals

evalPlusLoop :: Env -> Loop -> Either ForthErr Env
evalPlusLoop env loop =
  case _stack env of
    [] -> Left StackUnderflow
    [x] -> Left StackUnderflow
    (x : y : xs) -> Right (set stack xs env) >>= go x y (loop ^. loopbody)
  where
    go index stop forthvals env'
      | index == stop = Right env'
      | otherwise =
          case eval (save2Mem 0 index env') (Forthvals forthvals) of
            Left e -> Left e
            Right env'' -> case _stack env'' of
              [] -> Left StackUnderflow
              (0 : xs) -> Left SyntaxError
              (step : xs) -> if (step > 0 && (index + step) >= stop) || (step < 0 && (index + stop) <= stop) then Right (dropStackTop env'') else go (index + step) stop forthvals (dropStackTop env'')

evalUntilLoop :: Env -> Loop -> Either ForthErr Env
evalUntilLoop env loop = case eval env (Forthvals (loop ^. loopbody)) of
  Left err -> Left err
  Right newenv -> case _stack newenv of
    [] -> Left StackUnderflow
    (0 : xs) -> evalUntilLoop (dropStackTop newenv) loop
    (x : xs) -> Right $ dropStackTop newenv

evalVar :: Env -> T.Text -> Either ForthErr Env
evalVar env varname =
  Right $
    env
      { _names = Map.insert varname newaddress (_names env),
        _definitions = IM.insert newaddress (Number nextmemaddr) (_definitions env),
        _mem = IM.insert nextmemaddr 0 (_mem env)
      }
  where
    newaddress = Map.size (env ^. names)
    nextmemaddr = IM.size (env ^. mem) * env ^. memorycell

evalMemStore :: Env -> Either ForthErr Env
evalMemStore env =
  case _stack env of
    [] -> Left StackUnderflow
    [x] -> Left StackUnderflow
    x : y : zs -> Right $ dropStackTop . dropStackTop . save2Mem x y $ env

evalMemComma :: Env -> Either ForthErr Env
evalMemComma env =
  case _stack env of
    [] -> Left StackUnderflow
    [x] -> Left StackUnderflow
    x : y : zs ->
      Right $ save2Mem y x $ pushToStack (y + env ^. memorycell) $ dropStackTop $ dropStackTop env

-- this function will allocate contiguous memory to the array at the memory location regardless of whether it is already written

evalMemAllot :: Env -> Either ForthErr Env
evalMemAllot env =
  case _stack env of
    [] -> Left StackUnderflow
    [x] -> Left StackUnderflow
    x : y : zs -> Right $ env {_stack = zs, _mem = initializeMem xcells (env ^. mem)}
      where
        xcells = L.map ((+ y) . (* env ^. memorycell)) [0 .. x - 1]
        initializeMem cells dict =
          L.foldl' (\acc key -> IM.insert key 0 acc) dict cells

evalMemCellsize :: Env -> Either ForthErr Env
evalMemCellsize env =
  case _stack env of
    [] -> Left StackUnderflow
    x : xs -> Right $ pushToStack (x * env ^. memorycell) $ dropStackTop env

evalMemRetrieve :: Env -> Either ForthErr Env
evalMemRetrieve env =
  case _stack env of
    [] -> Left StackUnderflow
    x : zs ->
      case IM.lookup x (env ^. mem) of
        Nothing -> Left MemoryAccessError
        Just retrieved -> Right $ pushToStack retrieved $ dropStackTop env

evalMemStoreNext :: Env -> Either ForthErr Env
evalMemStoreNext env =
  case _stack env of
    [] -> Left StackUnderflow
    x : xs -> Right $ pushToStack nextMemAddr $ save2Mem nextMemAddr x env
      where
        nextMemAddr = IM.size (env ^. mem) * env ^. memorycell

lookupAll :: Text -> Env -> Maybe [Int]
lookupAll text env =
  traverse (`Map.lookup` (env ^. names)) $ T.split (== ' ') text

evalDefComponents :: Env -> ForthVal -> Maybe ForthVal
evalDefComponents env (Word text) = fmap Address $ Map.lookup text (env ^. names)
evalDefComponents env (If ifvals) = If <$> (evalDefBody env ifvals)
evalDefComponents env (IfElse ifvals elsevals) = IfElse <$> (evalDefBody env ifvals) <*> (evalDefBody env elsevals)
-- this needs to be modified for VARIABLE inside function definition (lokal variables)
evalDefComponents env Recurse = Just $ Address $ L.head $ env ^. stack
evalDefComponents env x = Just x

evalDefBody :: Env -> [ForthVal] -> Maybe [ForthVal]
evalDefBody env =
  traverse (evalDefComponents env)

evalInput :: String -> T.Text -> Env -> Either ForthErr Env
evalInput filename text env =
  case parseFromText filename text of
    Left e -> Left e
    Right forthvals -> L.foldl' eval' (Right env) forthvals
      where
        eval' eEnv forthval = eEnv >>= flip eval forthval

evalInputT :: String -> T.Text -> Env -> ExceptT ForthErr IO Env
evalInputT filename text env =
  case parseFromText filename text of
    Left e -> throwError e
    Right forthvals -> L.foldl' eval' (lift $ return env) forthvals
      where
        eval' eEnv forthval = eEnv >>= flip evalT forthval

evalInputRepl :: T.Text -> Env -> ExceptT ForthErr IO Env
evalInputRepl = evalInputT "repl"

evalAndPrintStackTop :: String -> T.Text -> Env -> IO ()
evalAndPrintStackTop filename text env =
  mapM_ printF $ evalInput filename text env

evalAndPrintDefDict :: String -> T.Text -> Env -> IO ()
evalAndPrintDefDict filename text env =
  case evalInput filename text env of
    Left e -> print e
    Right env' -> printDefs env'

evalAndPrintNames :: String -> T.Text -> Env -> IO ()
evalAndPrintNames filename text env =
  case evalInput filename text env of
    Left e -> print e
    Right env' -> printNames env'

evalAndPrintMemory :: String -> T.Text -> Env -> IO ()
evalAndPrintMemory filename text env =
  case evalInput filename text env of
    Left e -> print e
    Right env' -> printMem env'

evalAndPrintStackTopRepl :: Text -> Env -> IO ()
evalAndPrintStackTopRepl = evalAndPrintStackTop "repl"

evalFile :: String -> IO ()
evalFile filename = do
  contents <- readFile filename
  let text = T.unwords $ T.lines $ T.pack contents
  evalAndPrintStackTop filename text initialEnv

-- helper functions for record updates with lenses

dropStackTop :: Env -> Env
dropStackTop = over stack L.tail

pushToStack :: Int -> Env -> Env
pushToStack i = over stack (i :)

pushToPrintStack :: String -> Env -> Env
pushToPrintStack s = over printStr (s :)

dropfromPrintStack :: Env -> Env
dropfromPrintStack = over printStr L.tail

save2Mem :: Key -> Int -> Env -> Env
save2Mem addr val = over mem (IM.insert addr val)

invertStackTop :: Env -> Env
invertStackTop = over stack invertHead

-- partial function
invertHead :: (Eq a, Num a) => [a] -> [a]
invertHead (0 : xs) = 1 : xs
invertHead (x : xs) = 0 : xs
