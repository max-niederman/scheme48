{-# LANGUAGE BangPatterns #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import qualified Data.Bifunctor                 as Bifunctor
import qualified Data.List.NonEmpty             as NE
import           Lisp.Core
import           Lisp.Debug
import           Lisp.Interpret
import           Lisp.Parse
import           System.Environment             (getArgs)
import           System.IO
import           Text.PrettyPrint.HughesPJClass (prettyShow)

rep :: LispInterp ()
rep = do
  liftIO $ putStr "> " >> hFlush stdout
  source <- liftIO getLine
  evaled <- lift (parseLisp source) >>= eval
  liftIO (putStr "state:\n") >> get >>=
    liftIO . putStrLn . prettyShow . NE.toList
  liftIO $ putStrLn $ prettyShow evaled

repl :: LispExceptT IO ()
repl = evalStateT (forever rep) startState

file :: String -> [String] -> LispExceptT IO ()
file path _ = do
  source <- liftIO $ readFile path
  parsed <- parseLispMany source
  evaled <- evalStateT (mapM eval parsed) startState
  return ()

-- "root" as in the root of the argument trie
root :: [String] -> LispExceptT IO ()
root []            = repl
root ("repl":_)    = repl
root (filepath:as) = file filepath as

catchLispError :: LispExceptT IO a -> IO a
catchLispError = runExceptT >=> either (fail . show) return

main :: IO ()
main = getArgs >>= catchLispError . root
