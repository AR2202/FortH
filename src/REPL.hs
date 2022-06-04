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
  if input == ":q"
    then putStrLn "Goodbye"
    else mapM_ printF newenv >>
         case newenv of
           Left e    -> print e >> repl' env
           Right new -> repl' new

repl = repl' initialEnv
