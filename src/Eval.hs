{-# LANGUAGE OverloadedStrings #-}

module Eval
  ( initialDefs,
    initialEnv,
    initialNames,
    printF,
    printStack,
    eval,
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
        "EMIT"       
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
        Ascii
      ]

initialEnv :: Env
initialEnv = Env initialNames initialDefs [] (IM.singleton 0 0) 1 []

printF :: Env -> IO ()
printF env =
  case stack env of
    [] -> return ()
    (x : xs) -> print x

printStack :: Env -> IO ()
printStack env = mapM_ print $ L.reverse $ stack env

printDefs :: Env -> IO ()
printDefs env = print $ definitions env

printNames :: Env -> IO ()
printNames env = print $ names env

printMem :: Env -> IO ()
printMem env = print $ mem env

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

-- a test for ExceptT

evalT :: Env -> ForthVal -> ExceptT ForthErr IO Env
evalT env (SourceFile f) = ExceptT $ catchJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing) (evalFile f >> return (Right env)) (\_ -> return (Left (FileNotFound f)))
evalT env val = ExceptT $ return $ eval env val

evalStoreStr :: Env -> String -> Either ForthErr Env
evalStoreStr env s =
  case traverse ascii2num s of
    Nothing -> Left NonAsciiCode
    Just ns ->
      Right $
        env
          { stack = L.length s : nextmemaddr (mem env) : stack env,
            mem = L.foldl' (\mem' n -> IM.insert (nextmemaddr mem') n mem') (mem env) ns
          }
  where
    nextmemaddr mem' = IM.size mem' * memorycell env

evalType :: Env -> Either ForthErr Env
evalType env = case stack env of
  [] -> Left StackUnderflow
  (x : xs) -> fetchStr x env {stack = xs} ""

fetchStr :: (Eq t, Num t) => t -> Env -> [Char] -> Either ForthErr Env
fetchStr 0 env s = Right $ env {printStr = L.reverse s : printStr env}
fetchStr n env s = case evalDup env >>= evalMemRetrieve of
  Left err -> Left err
  Right env' -> case stack env' of
    [] -> Left StackUnderflow
    [x] -> Left StackUnderflow
    (x : y : xs) -> case num2ascii x of
      Nothing -> Left NonAsciiCode
      Just c -> fetchStr (n - 1) (env' {stack = y + memorycell env : xs}) (c : s)

evalKey :: Env -> Char -> Either ForthErr Env
evalKey env c = case ascii2num c of
  Nothing -> Left NonAsciiCode
  Just n -> Right $ env {stack = n : stack env}

evalAscii :: Env -> Either ForthErr Env
evalAscii env = case stack env of
  [] -> Left StackUnderflow
  (x : xs) -> case num2ascii x of
    Nothing -> Left NonAsciiCode
    Just c -> Right $ env {stack = xs, printStr = return c : printStr env}

evalNum :: Env -> Int -> Either ForthErr Env
evalNum env x = Right env {stack = x : stack env}

evalNot :: Env -> Either ForthErr Env
evalNot env =
  case stack env of
    [] -> Left StackUnderflow
    0 : xs -> Right $ env {stack = 1 : xs}
    x : xs -> Right $ env {stack = 0 : xs}

evalOp :: Env -> Operator -> Either ForthErr Env
evalOp env op =
  case stack env of
    [] -> Left StackUnderflow
    [x] -> Left StackUnderflow
    x : y : zs -> Right $ env {stack = operation op y x : zs}

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
  case stack env of
    [] -> Left StackUnderflow
    (x : xs) -> Right $ env {stack = x : x : xs}

evalDrop :: Env -> Either ForthErr Env
evalDrop env =
  case stack env of
    [] -> Left StackUnderflow
    (x : xs) -> Right $ env {stack = xs}

evalSwap :: Env -> Either ForthErr Env
evalSwap env =
  case stack env of
    [] -> Left StackUnderflow
    [x] -> Left StackUnderflow
    x : y : zs -> Right $ env {stack = y : x : zs}

evalOver :: Env -> Either ForthErr Env
evalOver env =
  case stack env of
    [] -> Left StackUnderflow
    [x] -> Left StackUnderflow
    x : y : zs -> Right $ env {stack = x : y : x : zs}

evalRot :: Env -> Either ForthErr Env
evalRot env =
  case stack env of
    [] -> Left StackUnderflow
    [x] -> Left StackUnderflow
    x : [y] -> Left StackUnderflow
    x : y : z : zs -> Right $ env {stack = y : z : x : zs}

evalInvert :: Env -> Either ForthErr Env
evalInvert env =
  case stack env of
    [] -> Left StackUnderflow
    0 : xs -> Right $ env {stack = 1 : xs}
    x : xs -> Right $ env {stack = 0 : xs}

evalPrint :: Env -> Either ForthErr Env
evalPrint env =
  case stack env of
    [] -> Left StackUnderflow
    (x : xs) -> Right $ env {stack = xs, printStr = show x : printStr env}

evalPrintString :: Env -> T.Text -> Either ForthErr Env
evalPrintString env t = Right $ env {printStr = T.unpack t : printStr env}

evalWord :: Env -> T.Text -> Either ForthErr Env
evalWord env name =
  case Map.lookup name (names env) of
    Nothing -> Left UnknownWord
    Just i ->
      case IM.lookup i (definitions env) of
        Nothing -> Left UnknownWord
        Just forthval -> eval env forthval

evalAddress :: Env -> Int -> Either ForthErr Env
evalAddress env i =
  case IM.lookup i (definitions env) of
    Nothing -> Left UnknownWord
    Just forthval -> eval env forthval

evalLookup :: Env -> Either ForthErr Env
evalLookup env =
  case stack env of
    [] -> Left StackUnderflow
    (x : xs) -> eval (env {stack = xs}) (Address x)

evalNameLookup :: Env -> T.Text -> Either ForthErr Env
evalNameLookup env name =
  case Map.lookup name (names env) of
    Nothing -> Left UnknownWord
    Just i -> Right $ env {stack = i : stack env}

evalDef :: Env -> Fun -> Either ForthErr Env
evalDef env fun =
  case evalDefBody env (body fun) of
    Nothing -> Left UnknownWord
    Just forthvals ->
      Right $
        env
          { names = Map.insert newword newaddress (names env),
            definitions =
              IM.insert newaddress (Forthvals forthvals) (definitions env)
          }
      where
        newword = name fun
        newaddress = Map.size (names env) + 1

evalForthvals env forthvals = L.foldl' eval' (Right env) forthvals
  where
    eval' eEnv forthval =
      case eEnv of
        Left error -> Left error
        Right env -> eval env forthval

evalIf :: Env -> [ForthVal] -> Either ForthErr Env
evalIf env forthvals =
  case stack env of
    [] -> Left StackUnderflow
    0 : xs -> Right env
    _ -> eval env (Forthvals forthvals)

evalIfElse :: Env -> [ForthVal] -> [ForthVal] -> Either ForthErr Env
evalIfElse env ifvals elsevals =
  case stack env of
    [] -> Left StackUnderflow
    0 : xs -> eval env (Forthvals elsevals)
    _ -> eval env (Forthvals ifvals)

evalDoLoop :: Env -> Loop -> Either ForthErr Env
evalDoLoop env loop =
  case stack env of
    [] -> Left StackUnderflow
    [x] -> Left StackUnderflow
    (x : y : zs) -> Right (env {mem = IM.insert 0 x (mem env), stack = zs}) >>= go x y (loopbody loop)
  where
    go index stop forthvals env'
      | index >= stop = Right env'
      | otherwise = eval (env' {mem = IM.insert 0 index (mem env)}) (Forthvals forthvals) >>= go (index + 1) stop forthvals

evalPlusLoop :: Env -> Loop -> Either ForthErr Env
evalPlusLoop env loop =
  case stack env of
    [] -> Left StackUnderflow
    [x] -> Left StackUnderflow
    (x : y : xs) -> Right (env {mem = IM.insert 0 x (mem env), stack = xs}) >>= go x y (loopbody loop)
  where
    go index stop forthvals env' =
      case stack env' of
        [] -> Left StackUnderflow
        (0 : xs) -> Left SyntaxError
        (step : xs) -> if (step > 0 && index >= stop) || (step < 0 && index < stop) then Right env' else newenv >>= go (index + step) stop forthvals
      where
        newenv = eval (env' {mem = addedIndex}) (Forthvals forthvals)
        addedIndex = IM.insert 0 index $ mem env'

evalUntilLoop env loop = case eval env (Forthvals (loopbody loop)) of
  Left err -> Left err
  Right newenv -> case stack newenv of
    [] -> Left StackUnderflow
    (0 : xs) -> evalUntilLoop (newenv {stack = xs}) loop
    (x : xs) -> Right (newenv {stack = xs})

evalVar :: Env -> T.Text -> Either ForthErr Env
evalVar env varname =
  Right $
    env
      { names = Map.insert varname newaddress (names env),
        definitions = IM.insert newaddress (Number nextmemaddr) (definitions env),
        mem = IM.insert nextmemaddr 0 (mem env)
      }
  where
    newaddress = Map.size (names env)
    nextmemaddr = IM.size (mem env) * memorycell env

evalMemStore :: Env -> Either ForthErr Env
evalMemStore env =
  case stack env of
    [] -> Left StackUnderflow
    [x] -> Left StackUnderflow
    x : y : zs -> Right $ env {stack = zs, mem = IM.insert x y (mem env)}

evalMemComma :: Env -> Either ForthErr Env
evalMemComma env =
  case stack env of
    [] -> Left StackUnderflow
    [x] -> Left StackUnderflow
    x : y : zs ->
      Right $
        env {stack = (y + memorycell env) : zs, mem = IM.insert y x (mem env)}

-- this function will allocate contiguous memory to the array at the memory location regardless of whether it is already written

evalMemAllot :: Env -> Either ForthErr Env
evalMemAllot env =
  case stack env of
    [] -> Left StackUnderflow
    [x] -> Left StackUnderflow
    x : y : zs -> Right $ env {stack = zs, mem = initializeMem xcells (mem env)}
      where
        xcells = L.map ((+ y) . (* memorycell env)) [1 .. x]
        initializeMem cells dict =
          L.foldl' (\acc key -> IM.insert key 0 acc) dict cells

evalMemCellsize :: Env -> Either ForthErr Env
evalMemCellsize env =
  case stack env of
    [] -> Left StackUnderflow
    x : xs -> Right $ env {stack = (x * memorycell env) : xs}

evalMemRetrieve :: Env -> Either ForthErr Env
evalMemRetrieve env =
  case stack env of
    [] -> Left StackUnderflow
    x : zs ->
      case IM.lookup x (mem env) of
        Nothing -> Left MemoryAccessError
        Just retrieved -> Right $ env {stack = retrieved : zs}

evalMemStoreNext :: Env -> Either ForthErr Env
evalMemStoreNext env =
  case stack env of
    [] -> Left StackUnderflow
    x : xs -> Right $ env {stack = nextMemAddr : xs, mem = IM.insert nextMemAddr x (mem env)}
      where
        nextMemAddr = IM.size (mem env) * memorycell env

lookupAll :: Text -> Env -> Maybe [Int]
lookupAll text env =
  sequenceA $ Prelude.map (flip Map.lookup (names env)) $ T.split (== ' ') text

evalDefComponents env (Word text) = fmap Address $ Map.lookup text (names env)
evalDefComponents env x = Just x

evalDefBody :: Env -> [ForthVal] -> Maybe [ForthVal]
evalDefBody env forthvals =
  sequenceA $ Prelude.map (evalDefComponents env) forthvals

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

evalAndPrintStackTopRepl = evalAndPrintStackTop "repl"

evalFile :: String -> IO ()
evalFile filename = do
  contents <- readFile filename
  let text = T.unwords $ T.lines $ T.pack contents
  evalAndPrintStackTop filename text initialEnv
