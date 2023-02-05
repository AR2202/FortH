{-# LANGUAGE OverloadedStrings #-}

module REPL
  ( repl'
  , repl
  ) where

import           Data.Text as T
import           Eval
import           ForthVal
import           Parser

repl' :: Env -> IO ()
repl' env = do
  putStr "FortHi> "
  input <- getLine
  let newenv = evalInputRepl (T.pack input) env
  case input of
    ":q" -> putStrLn "Goodbye"
    ".s" -> printStack env >> repl' env
    _ ->
      case newenv of
        Left e    -> print e >> repl' env
        Right new -> putStrLn "ok" >> printF new >> repl' new

repl :: IO ()
repl = do
  putStrLn "Welcome to FortHi"
  putStrLn "type ':q' to exit"
  repl' initialEnv
