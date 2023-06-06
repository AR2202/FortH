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
  putStrLn "FortHi> "
    
  input <- getLine
 
  case input of
    ":q" -> putStrLn "Goodbye"
    ".s" -> printStack env >> repl' env
    _ ->
      case evalInputRepl (T.pack input) env of
        Left e    -> print e >> repl' env
        Right new ->  putStrLn "ok" >> mapM_ putStrLn ((Prelude.reverse . printStr) new) >> printF new >>repl' new {printStr = []}

repl :: IO ()
repl = do
  putStrLn "Welcome to FortHi"
  putStrLn "type ':q' to exit"
  
  repl' initialEnv
