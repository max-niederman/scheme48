module Vanessa.Core where

data LispVal
  = Symbol String
  | Pair [LispVal]
  | Number Integer
  | String String
  | Bool Bool
  deriving (Eq)

data LispError
  = ParseError String
  | NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | UnboundVar String

type Fallible = Either LispError

instance Show LispVal where
  show (Symbol name)   = name
  show (Pair contents) = "(" ++ unwords (map show contents) ++ ")"
  show (Number n)      = show n
  show (String s)      = "\"" ++ s ++ "\""
  show (Bool True)     = "#t"
  show (Bool False)    = "#f"

instance Show LispError where
  show (ParseError message) = "Parse error: " ++ message
  show (NumArgs expected arguments) =
    "Expected " ++ show expected ++ " arguments, but got " ++ show (length arguments)
  show (TypeMismatch expected actual) =
    "Expected " ++ expected ++ ", but got " ++ show actual
  show (UnboundVar name) =
    "Variable '" ++ name ++ "' is unbound"
