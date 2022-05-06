module Vanessa.Value where

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

unpackPair :: LispVal -> Fallible [LispVal]
unpackPair (Pair n) = return n
unpackPair val      = Left $ TypeMismatch "pair" val

unpackPairToCons :: LispVal -> Fallible (LispVal, LispVal)
unpackPairToCons (Pair (car:cdr)) = return (car, Pair cdr)
unpackPairToCons val              = Left $ TypeMismatch "pair" val

unpackNumber :: LispVal -> Fallible Integer
unpackNumber (Number n) = return n
unpackNumber val        = Left $ TypeMismatch "number" val

unpackBool :: LispVal -> Fallible Bool
unpackBool (Bool b) = return b
unpackBool val      = Left $ TypeMismatch "boolean" val
