module Vanessa.Value where

import           Control.Monad.Trans.Except
import           Vanessa.Core

isSymbol :: LispVal -> Bool
isSymbol (Symbol _) = True
isSymbol _          = False

isPair :: LispVal -> Bool
isPair (Pair _) = True
isPair _        = False

isNumber :: LispVal -> Bool
isNumber (Number _) = True
isNumber _          = False

isString :: LispVal -> Bool
isString (String _) = True
isString _          = False

isBoolean :: LispVal -> Bool
isBoolean (Bool _) = True
isBoolean _        = False

unpackSymbol :: LispVal -> LispExcept String
unpackSymbol (Symbol s) = return s
unpackSymbol val        = throwE $ TypeMismatch "symbol" val

unpackPair :: LispVal -> LispExcept [LispVal]
unpackPair (Pair n) = return n
unpackPair val      = throwE $ TypeMismatch "pair" val

unpackPairToCons :: LispVal -> LispExcept (LispVal, LispVal)
unpackPairToCons (Pair (car:cdr)) = return (car, Pair cdr)
unpackPairToCons val              = throwE $ TypeMismatch "pair" val

unpackNumber :: LispVal -> LispExcept Integer
unpackNumber (Number n) = return n
unpackNumber val        = throwE $ TypeMismatch "number" val

unpackBool :: LispVal -> LispExcept Bool
unpackBool (Bool b) = return b
unpackBool val      = throwE $ TypeMismatch "boolean" val
