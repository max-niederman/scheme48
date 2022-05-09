module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import qualified Data.Bifunctor             as Bifunctor
import qualified Data.List.NonEmpty         as NE
import           System.Environment         (getArgs)
import           System.IO
import           Vanessa.Core
import           Vanessa.Eval
import           Vanessa.Parse              (parseLisp)

rep :: LispState ()
rep = do
  liftIO $ putStr "vanessa > " >> hFlush stdout

  source <- liftIO getLine
  parsed <- lift $ parseLisp source
  evaled <- eval parsed

  liftIO (putStr "state: ") >> get >>= liftIO . print . NE.toList
  liftIO $ print evaled

repl :: LispExceptT IO ()
repl = evalStateT (forever rep) startEnv

-- "root" as in the root of the argument trie
root :: [String] -> LispExceptT IO ()
root []         = repl
root ("repl":_) = repl
root (file:_)   = error "todo: load file"

catchLispError :: LispExceptT IO a -> IO a
catchLispError = runExceptT >=> either (fail . show) return

main :: IO ()
main = getArgs >>= (catchLispError . root)
