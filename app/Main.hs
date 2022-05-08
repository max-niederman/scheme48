module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import qualified Data.Bifunctor             as Bifunctor
import           System.Environment         (getArgs)
import           System.IO
import           Vanessa.Core
import           Vanessa.Eval
import           Vanessa.Parse              (parseLisp)

catchLispError :: LispExceptT IO a -> IO a
catchLispError = runExceptT >=> either (fail . show) return

rep :: LispState ()
rep = do
  liftIO $ putStr "vanessa > " >> hFlush stdout
  source <- liftIO getLine
  parsed <- lift $ parseLisp source
  evaled <- eval parsed
  liftIO $ print evaled

repl :: LispExceptT IO ()
repl = evalStateT (forever rep) startEnv

-- "root" as in the root of the argument trie
root :: [String] -> LispExceptT IO ()
root []         = repl
root ("repl":_) = repl
root (file:_) = error "todo: load file"

main :: IO ()
main = getArgs >>= (catchLispError . root)
