module Vanessa.Eval (eval, LispEnv, startEnv, LispState) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Data.Either                (fromRight)
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map.Lazy              as Map
import           Data.Monoid                (First (First, getFirst))
import           Vanessa.Core
import           Vanessa.Value

-- an environment in which an expression may be executed
-- reversed list of scopes, each containing a map of identifiers to values
type LispEnv = NE.NonEmpty LispScope
type LispScope = Map.Map String LispVal

startEnv :: LispEnv
startEnv = Map.empty NE.:| []

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

eval (Pair [Symbol "defined?", Symbol id])    = Bool <$> isDefined id
eval (Pair [Symbol "define", Symbol id, val]) = eval val >>= defineVar id >> return val
eval (Pair [Symbol "define", id, _])          = lift . throwE $ TypeMismatch "symbol" id
eval (Pair (Symbol "define":args))            = lift . throwE $ NumArgs 2 args
eval (Pair [Symbol "set!", Symbol id, val])   = eval val >>= setVar id >> return val
eval (Pair [Symbol "set!", id, _])            = lift . throwE $ TypeMismatch "symbol" id
eval (Pair (Symbol "set!":args))              = lift . throwE $ NumArgs 2 args
eval (Symbol id)                              = getVar id >>= maybe (lift . throwE $ UnboundVar id) return

eval (Pair [Symbol "print", expr]) = do
  val <- eval expr
  liftIO $ print val
  return $ Pair []

eval (Pair (Symbol func : args)) = mapM eval args >>= apply func

eval val@(Pair _)   = return val
eval val@(Number _) = return val
eval val@(String _) = return val
eval val@(Bool _)   = return val

defineVar :: String -> LispVal -> LispState ()
defineVar id val = do
  env <- get
  let old = NE.head env
  let new = Map.insert id val old
  put $ new NE.:| NE.tail env

setVar :: String -> LispVal -> LispState ()
setVar id val = get >>= lift . returnInExcept . setVar' . NE.toList >>= put
  where
    setVar' :: [LispScope] -> LispExcept LispEnv
    setVar' [] = throwE $ UnboundVar id
    setVar' (scope:scopes) = if Map.member id scope
      then return $ Map.insert id val scope NE.:| scopes
      else setVar' scopes

getVar :: String -> LispState (Maybe LispVal)
getVar id = getFirst . foldMap (First . Map.lookup id) <$> get

isDefined :: String -> LispState Bool
isDefined id = any (Map.member id) <$> get

pushScope :: LispScope -> LispState ()
pushScope scope = modify $ NE.cons scope

pushEmptyScope :: LispState ()
pushEmptyScope = pushScope Map.empty

popScope :: LispState ()
popScope = do
  env <- get
  case NE.tail env of
    []   -> lift . throwE $ Internal "popScope: empty environment"
    s:ss -> put $ s NE.:| ss

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
