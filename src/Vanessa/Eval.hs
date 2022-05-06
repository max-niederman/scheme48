module Vanessa.Eval (eval) where

import           Data.Either   (fromRight)
import           Vanessa.Core
import           Vanessa.Value

eval :: LispVal -> Fallible LispVal
eval (Pair [Symbol "quote", val]) = return val
eval (Pair [Symbol "if", pred, conseq, alt]) = do
  pred' <- eval pred
  case pred' of
    Bool True  -> eval conseq
    Bool False -> eval alt
    _          -> Left $ TypeMismatch "boolean" pred'
eval (Pair (Symbol func : args))  = mapM eval args >>= apply func

eval val@(Symbol _)               = return val
eval val@(Pair _)                = return val
eval val@(Number _)               = return val
eval val@(String _)               = return val
eval val@(Bool _)                 = return val

apply :: String -> [LispVal] -> Fallible LispVal
apply name args = case lookup name builtinFunctions of
  Just f  -> f args
  Nothing -> Left $ UnboundVar name

builtinFunctions :: [(String, [LispVal] -> Fallible LispVal)]
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

    equalityOp :: (LispVal -> LispVal -> Bool) -> [LispVal] -> Fallible LispVal
    equalityOp f []     = return $ Bool True
    equalityOp f (x:xs) = return . Bool $ all (f x) xs

    typeCheck :: (LispVal -> Bool) -> [LispVal] -> Fallible LispVal
    typeCheck f args = return . Bool $ all f args

    unaryOp :: (LispVal -> Fallible a) -> (r -> LispVal) -> (a -> r) -> [LispVal] -> Fallible LispVal
    unaryOp unpack con f [a] = con . f <$> unpack a
    unaryOp _ _ _ args       = Left $ NumArgs 1 args

    binaryOp :: (LispVal -> Fallible a) -> (r -> LispVal) -> (a -> a -> r) -> [LispVal] -> Fallible LispVal
    binaryOp unpack con f [a, b] = do
      a' <- unpack a
      b' <- unpack b
      return . con $ f a' b'
    binaryOp _ _ _ args = Left $ NumArgs 2 args

    variadicFoldingOp :: (LispVal -> Fallible a) -> (a -> LispVal) -> (a -> a -> a) -> a -> [LispVal] -> Fallible LispVal
    variadicFoldingOp unpack cons op identity params = return . cons $ foldl op identity $ map (fromRight identity . unpack) params

    -- we can't use `binaryOp` for this because the unpacking is different for `car` and `cdr`
    consOp :: [LispVal] -> Fallible LispVal
    consOp [car, Pair cdr] = return $ Pair (car : cdr)
    consOp [_, cdr]        = Left $ TypeMismatch "pair" cdr
    consOp args            = Left $ NumArgs 2 args
