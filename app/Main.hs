module Main where

import           Control.Monad
import qualified Data.Bifunctor     as Bifunctor
import           System.Environment (getArgs)
import           System.IO
import           Vanessa.Core
import           Vanessa.Eval
import           Vanessa.Parse      (parseLisp)

evalAndPrint :: String -> IO ()
evalAndPrint = either (putStrLn . ("Error:\n" ++) . show) print . (eval <=< parseLisp)

repl :: IO ()
repl = forever $ do
  putStr "vanessa > "
  hFlush stdout
  getLine >>= evalAndPrint

root :: [String] -> IO ()
root [] = repl
root ("repl":_) = repl
root (file:_) = do
  source <- readFile file
  evalAndPrint source

main :: IO ()
main = getArgs >>= root
