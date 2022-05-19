module Lisp.Core where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except (Except, ExceptT)
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.State
import           Data.Functor.Identity
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map.Lazy              as Map
import qualified Data.Set                   as Set
import           GHC.IO.Handle              (Handle)

data LispVal
  = Symbol String
  | Pair [LispVal]
  | Number Integer
  | String String
  | Bool Bool
  | Func
      { param   :: LispParam
      , body    :: LispVal
      , closure :: Map.Map String LispVal
      }
  | PrimFunc ([LispVal] -> LispInterp LispVal)
  | Port Handle

data LispParam
  = NaryParam [String]
  | VariadicParam String

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
returnInExcept = E.mapExceptT $ return . runIdentity

-- reversed list of scopes, each containing a map of identifiers to values
type LispState = NE.NonEmpty LispScope

type LispScope = Map.Map String LispVal

-- interpreter monad handling I/O, error handling, and state
type LispInterp = StateT LispState (LispExceptT IO)

-- lifted operations
throwE :: LispError -> LispInterp a
throwE = lift . E.throwE

catchE :: LispInterp a -> (LispError -> LispInterp a) -> LispInterp a
catchE = liftCatch E.catchE
