{-# LANGUAGE OverloadedStrings #-}

module REPL
  ( repl',
    repl,
  )
where

import Control.Exception (catchJust, try, tryJust)
import Control.Monad.Except (ExceptT (..), catchError, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Text as T
import Eval
import ForthVal
import Parser
import System.IO.Error (ioeGetErrorType, isDoesNotExistErrorType)

repl' :: Env -> IO ()
repl' env = do
  putStrLn "FortHi> "

  input <- getLine

  case input of
    ":q" -> putStrLn "Goodbye"
    ".s" -> printStack env >> repl' env
    _ -> do
      evalResult <- runExceptT (evalInputRepl (T.pack input) env)

      case evalResult of
        Left e -> print e >> repl' env
        Right new -> putStrLn "ok" >> putStrLn ((Prelude.concat . Prelude.reverse . printStr) new) >> printF new >> repl' new {printStr = []}

repl :: IO ()
repl = do
  putStrLn "Welcome to FortHi"
  putStrLn "type ':q' to exit"

  repl' initialEnv
