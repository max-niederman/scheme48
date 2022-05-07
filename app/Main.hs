module Main where

import           Control.Monad
import qualified Data.Bifunctor     as Bifunctor
import           System.Environment (getArgs)
import           Vanessa.Core
import           Vanessa.Eval
import           Vanessa.Parse      (parseLisp)

readExpr :: String -> Fallible LispVal
readExpr input = Bifunctor.first (ParseError . show) $ parseLisp input

main :: IO ()
main = getArgs >>= sourceFromArgs >>= either (putStrLn . ("Error:\n" ++) . show) print . (eval <=< readExpr)
  where
    sourceFromArgs :: [String] -> IO String
    sourceFromArgs []    = readFile "/dev/stdin"
    sourceFromArgs (x:_) = readFile x
