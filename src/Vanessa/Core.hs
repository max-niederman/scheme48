module Vanessa.Core where
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Data.Functor.Identity
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map.Lazy              as Map

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
  | Internal String

type LispExceptT = ExceptT LispError
type LispExcept = LispExceptT Identity

-- useful for converting a `LispExcept` to a `LispExceptT m`
-- TODO: there is probably a better way to do this, but i can't find it in `transformers`
returnInExcept :: Monad m => Except e a -> ExceptT e m a
returnInExcept = mapExceptT (return . runIdentity)





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
  show (Internal message) =
    "Internal error: " ++ message
