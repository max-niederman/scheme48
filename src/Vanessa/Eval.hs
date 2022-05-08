module Vanessa.Eval (eval, LispEnv, startEnv, LispState) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Data.Either                (fromRight)
import           Data.Functor.Identity
import           Vanessa.Core
import           Vanessa.Value

-- an environment in which an expression may be executed
-- reversed list of scopes, each containing a map of bindings to values
type LispEnv = [[(String, LispVal)]]

startEnv :: LispEnv
startEnv = [[]]

type LispState = StateT LispEnv (LispExceptT IO)

-- evaluate a lisp expression and execute its side effects
eval :: LispVal -> LispState LispVal
eval (Pair [Symbol "quote", val]) = return val
eval (Pair [Symbol "if", pred, conseq, alt]) = do
  pred' <- eval pred
  case pred' of
    Bool True  -> eval conseq
    Bool False -> eval alt
    _          -> lift . throwE $ TypeMismatch "boolean" pred'
eval (Pair [Symbol "print", expr]) = do
  val <- eval expr
  liftIO $ print val
  return $ Pair []
eval (Pair (Symbol func : args))  = mapM eval args >>= apply func

eval val@(Symbol _)               = return val
eval val@(Pair _)                 = return val
eval val@(Number _)               = return val
eval val@(String _)               = return val
eval val@(Bool _)                 = return val

apply :: String -> [LispVal] -> LispState LispVal
apply name args = case lookup name builtinFunctions of
  Just f  -> lift $ f args
  Nothing -> lift . throwE $ UnboundVar name

builtinFunctions :: [(String, [LispVal] -> LispExceptT IO LispVal)]
builtinFunctions = [
    ("symbol?", typeCheck isSymbol),
    ("pair?", typeCheck isPair),
    ("number?", typeCheck isNumber),
    ("string?", typeCheck isString),
    ("boolean?", typeCheck isBoolean),

    ("eqv?", equalityOp (==)),

    ("car", unaryOp unpackPairToCons id fst),
    ("cdr", unaryOp unpackPairToCons id snd),
    ("cons", consOp),

    ("not",  unaryOp unpackBool Bool not),
    ("&&",  variadicFoldingOp unpackBool Bool (&&) True),
    ("||",  variadicFoldingOp unpackBool Bool (||) False),

    ("+",  variadicFoldingOp unpackNumber Number (+) 0),
    ("-",  variadicFoldingOp unpackNumber Number (-) 0),
    ("*",  variadicFoldingOp unpackNumber Number (*) 1),
    ("/",  variadicFoldingOp unpackNumber Number div 1),
    ("modulo",  binaryOp unpackNumber Number mod),
    ("quotient",  binaryOp unpackNumber Number quot),
    ("remainder",  binaryOp unpackNumber Number rem),

    ("=",  binaryOp unpackNumber Bool (==)),
    ("<",  binaryOp unpackNumber Bool (<)),
    (">",  binaryOp unpackNumber Bool (>)),
    ("/=",  binaryOp unpackNumber Bool (/=)),
    (">=",  binaryOp unpackNumber Bool (>=)),
    ("<=",  binaryOp unpackNumber Bool (<=))
  ]

  where
    -- TODO: improve error handling

    equalityOp :: (LispVal -> LispVal -> Bool) -> [LispVal] -> LispExceptT IO LispVal
    equalityOp f []     = return $ Bool True
    equalityOp f (x:xs) = return . Bool $ all (f x) xs

    typeCheck :: (LispVal -> Bool) -> [LispVal] -> LispExceptT IO LispVal
    typeCheck f args = return . Bool $ all f args

    unaryOp :: (LispVal -> LispExcept a) -> (r -> LispVal) -> (a -> r) -> [LispVal] -> LispExceptT IO LispVal
    unaryOp unpack con f [a] = returnInExcept $ con . f <$> unpack a
    unaryOp _ _ _ args       = throwE $ NumArgs 1 args

    binaryOp :: (LispVal -> LispExcept a) -> (r -> LispVal) -> (a -> a -> r) -> [LispVal] -> LispExceptT IO LispVal
    binaryOp unpack con f [a, b] = do
      a' <- returnInExcept $ unpack a
      b' <- returnInExcept $ unpack b
      return . con $ f a' b'
    binaryOp _ _ _ args = throwE $ NumArgs 2 args

    variadicFoldingOp :: (LispVal -> LispExcept a) -> (a -> LispVal) -> (a -> a -> a) -> a -> [LispVal] -> LispExceptT IO LispVal
    variadicFoldingOp unpack cons op identity params = return . cons $ foldl op identity $ map (fromRight identity . runExcept . unpack) params

    -- we can't use `binaryOp` for this because the unpacking is different for `car` and `cdr`
    consOp :: [LispVal] -> LispExceptT IO LispVal
    consOp [car, Pair cdr] = return $ Pair (car : cdr)
    consOp [_, cdr]        = throwE $ TypeMismatch "pair" cdr
    consOp args            = throwE $ NumArgs 2 args
