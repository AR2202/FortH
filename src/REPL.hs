{-# LANGUAGE OverloadedStrings #-}

module REPL
  ( repl',
    repl,
  )
where

import Control.Exception (catchJust, try, tryJust)
import Data.Text as T
import Eval
import ForthVal
import Parser
import System.IO.Error (ioeGetErrorType, isDoesNotExistErrorType)
import Control.Monad.Except (ExceptT (..), catchError, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)

repl' :: Env -> IO ()
repl' env = do
  putStrLn "FortHi> "

  input <- getLine

  case input of
    ":q" -> putStrLn "Goodbye"
    ".s" -> printStack env >> repl' env
    ':' : 'e' : ' ' : f -> catchJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing) (evalFile f >> repl' env) (\_ -> do putStrLn ("No such file: " ++ show f) >> repl' env)
    _ -> do
      evalResult <- runExceptT (evalInputRepl (T.pack input) env)

      case evalResult of
        Left e -> print e >> repl' env
        Right new -> putStrLn "ok" >> mapM_ putStrLn ((Prelude.reverse . printStr) new) >> printF new >> repl' new {printStr = []}

repl :: IO ()
repl = do
  putStrLn "Welcome to FortHi"
  putStrLn "type ':q' to exit"

  repl' initialEnv
