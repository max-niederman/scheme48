{-# LANGUAGE NamedFieldPuns #-}
module Vanessa.Core where
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Data.Functor.Identity
import qualified Data.Map.Lazy              as Map
import qualified Data.Set                   as Set

data LispVal
  = Symbol String
  | Pair [LispVal]
  | Number Integer
  | String String
  | Bool Bool
  | Func { param   :: LispParam
         , body    :: LispVal
         , closure :: Map.Map String LispVal
         }
  | PrimFunc ([LispVal] -> LispExceptT IO LispVal)

data LispParam = NaryParam [String] | VariadicParam String

instance Eq LispVal where
  (==) (Symbol a) (Symbol b) = a == b
  (==) (Pair a) (Pair b)     = a == b
  (==) (Number a) (Number b) = a == b
  (==) (String a) (String b) = a == b
  (==) (Bool a) (Bool b)     = a == b
  -- functions, including primitive functions, are never equal
  (==) _ _                   = False

data LispError
  = ParseError String
  | NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | UnboundVar String
  | Internal String

type LispExceptT = ExceptT LispError
type LispExcept = LispExceptT Identity

-- useful for converting a `LispExcept` to a `LispExceptT m`
-- TODO: there is probably a better way to do this, but i can't find it in `transformers`
returnInExcept :: Monad m => Except e a -> ExceptT e m a
returnInExcept = mapExceptT (return . runIdentity)

instance Show LispVal where
  show (Symbol name)        = name
  show (Pair contents)      = "(" ++ unwords (map show contents) ++ ")"
  show (Number n)           = show n
  show (String s)           = "\"" ++ s ++ "\""
  show (Bool True)          = "#t"
  show (Bool False)         = "#f"
  show Func { param, body, closure } = "(let (" ++ unwords (map show $ Map.toList closure) ++ ") (lambda " ++ show param ++ " " ++ show body ++ "))"
  show (PrimFunc _)         = "\\primitive function\\"

instance Show LispParam where
  show (NaryParam ps)    = show $ Pair $ map Symbol ps
  show (VariadicParam p) = p

instance Show LispError where
  show (ParseError message) = "Parse error: " ++ message
  show (NumArgs expected arguments) =
    "Expected " ++ show expected ++ " arguments, but got " ++ show (length arguments)
  show (TypeMismatch expected actual) =
    "Expected " ++ expected ++ ", but got " ++ show actual
  show (UnboundVar name) =
    "Variable '" ++ name ++ "' is unbound"
  show (Internal message) =
    "Internal error: " ++ message
