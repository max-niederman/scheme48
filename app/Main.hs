module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import qualified Data.Bifunctor                 as Bifunctor
import qualified Data.List.NonEmpty             as NE
import           System.Environment             (getArgs)
import           System.IO
import           Text.PrettyPrint.HughesPJClass (prettyShow)
import           Vanessa.Core
import           Vanessa.Debug
import           Vanessa.Interpret
import           Vanessa.Parse                  (parseLisp)

rep :: LispInterp ()
rep = do
  liftIO $ putStr "> " >> hFlush stdout
  source <- liftIO getLine
  parsed <- lift $ parseLisp source
  evaled <- eval parsed
  liftIO (putStr "state:\n") >> get >>=
    liftIO . putStrLn . prettyShow . NE.toList
  liftIO $ putStrLn $ prettyShow evaled

repl :: LispExceptT IO ()
repl = evalStateT (forever rep) startState

-- "root" as in the root of the argument trie
root :: [String] -> LispExceptT IO ()
root []         = repl
root ("repl":_) = repl
root (file:_)   = error "todo: load file"

catchLispError :: LispExceptT IO a -> IO a
catchLispError = runExceptT >=> either (fail . show) return

main :: IO ()
main = getArgs >>= (catchLispError . root)
