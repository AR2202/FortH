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
  )
where

import Data.IntMap as IM
import Data.List as L
import qualified Data.Map as Map
import Data.Text as T
import ForthVal
import Parser

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
        "XOR"
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
        Arith Xor
      ]

initialEnv :: Env
initialEnv = Env initialNames initialDefs [] IM.empty 1

printF :: Env -> IO ()
printF env =
  case stack env of
    [] -> return ()
    (x : xs) -> print x

printStack :: Env -> IO ()
printStack env = mapM_ print $ L.reverse $ stack env

eval :: Env -> ForthVal -> Either ForthErr Env
eval env (Number x) = evalNum env x
eval env (Arith op) = evalOp env op
eval env (Manip Dup) = evalDup env
eval env (Manip Drop) = evalDrop env
eval env (Manip Swap) = evalSwap env
eval env (Manip Over) = evalOver env
eval env (Manip Rot) = evalRot env
eval env (Manip Invert) = evalInvert env
eval env (Word name) = evalWord env name
eval env (Address i) = evalAddress env i
eval env (Def fun) = evalDef env fun
eval env (Forthvals forthvals) = evalForthvals env forthvals
eval env (If forthvals) = evalIf env forthvals
eval env (IfElse ifvals elsevals) = evalIfElse env ifvals elsevals
eval env (DoLoop loop) = evalDoLoop env loop
eval env (PlusLoop loop) = evalPlusLoop env loop
eval env (Variable varname) = evalVar env varname
eval env (Mem Store) = evalMemStore env
eval env (Mem Retrieve) = evalMemRetrieve env
eval env (Mem Allot) = evalMemAllot env
eval env (Mem Cellsize) = evalMemCellsize env
eval env (Mem CommaStore) = evalMemComma env

evalNum :: Env -> Int -> Either ForthErr Env
evalNum env x = Right env {stack = x : stack env}

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
        newaddress = Map.size (names env)

evalForthvals env forthvals = L.foldl' eval' (Right env) forthvals
  where
    eval' eEnv forthval =
      case eEnv of
        Left error -> Left error
        Right env -> eval env forthval

evalIf env forthvals =
  case stack env of
    [] -> Left StackUnderflow
    0 : xs -> Right env
    _ -> eval env (Forthvals forthvals)

evalIfElse env ifvals elsevals =
  case stack env of
    [] -> Left StackUnderflow
    0 : xs -> eval env (Forthvals elsevals)
    _ -> eval env (Forthvals ifvals)

evalDoLoop env loop = go (Right env) (start loop) (loopbody loop)
  where
    go (Left err) _ _ = Left err
    go (Right env') index forthvals
      | index >= stop loop = Right env'
      | otherwise = go (eval env' (Forthvals forthvals)) (index + 1) forthvals

evalPlusLoop env loop =
  case stack env of
    [] -> Left StackUnderflow
    (x : xs) -> go (Right env) (start loop) x (loopbody loop)
  where
    go (Left err) _ _ _ = Left err
    go (Right env') index step forthvals
      | step == 0 = Left SyntaxError
      | (step > 0 && index >= stop loop) || (step < 0 && index < stop loop) =
          Right env'
      | (stack <$> newenv) == Right [] = Left StackUnderflow
      | otherwise = go newenv (index + step) newstep forthvals
      where
        newenv = eval env' (Forthvals forthvals)
        newstep =
          case newenv of
            Left _ -> 0
            Right newenv' -> L.head $ stack newenv'

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

lookupAll text env =
  sequenceA $ Prelude.map (flip Map.lookup (names env)) $ T.split (== ' ') text

evalDefComponents env (Number i) = Just (Number i)
evalDefComponents env (Word text) = fmap Address $ Map.lookup text (names env)

evalDefBody env forthvals =
  sequenceA $ Prelude.map (evalDefComponents env) forthvals

evalInput :: String -> T.Text -> Env -> Either ForthErr Env
evalInput filename text env =
  case parseFromText filename text of
    Left e -> Left e
    Right forthvals -> L.foldl' eval' (Right env) forthvals
      where
        eval' eEnv forthval =
          case eEnv of
            Left error -> Left error
            Right env -> eval env forthval

evalInputRepl :: T.Text -> Env -> Either ForthErr Env
evalInputRepl = evalInput "repl"

evalAndPrintStackTop :: String -> T.Text -> Env -> IO ()
evalAndPrintStackTop filename text env =
  mapM_ printF $ evalInput filename text env

evalAndPrintStackTopRepl = evalAndPrintStackTop "repl"

evalFile :: String -> IO ()
evalFile filename = do
  contents <- readFile filename
  let text = T.unwords $ T.lines $ T.pack contents
  evalAndPrintStackTop filename text initialEnv
