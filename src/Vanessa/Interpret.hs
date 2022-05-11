{-# LANGUAGE NamedFieldPuns #-}

module Vanessa.Interpret (eval, LispScope, LispState, startState, LispInterp) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except (runExcept)
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.State
import           Data.Either                (fromRight, rights)
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map.Lazy              as Map
import           Data.Monoid                (First (First, getFirst))
import qualified Data.Set                   as Set
import           Vanessa.Core
import           Vanessa.Value

-- reversed list of scopes, each containing a map of identifiers to values
type LispState = NE.NonEmpty LispScope
type LispScope = Map.Map String LispVal

startState :: LispState
startState = Map.empty NE.:| []

-- interpreter monad handling I/O, error handling, and state
type LispInterp = StateT LispState (LispExceptT IO)

-- lifted operations

throwE :: LispError -> LispInterp a
throwE = lift . E.throwE

catchE :: LispInterp a -> (LispError -> LispInterp a) -> LispInterp a
catchE = liftCatch E.catchE

-- evaluate a lisp expression and execute its side effects
eval :: LispVal -> LispInterp LispVal

eval (Pair [Symbol "quote", val]) = return val

eval (Pair [Symbol "if", pred, conseq, alt]) = do
  pred' <- eval pred
  case pred' of
    Bool True  -> eval conseq
    Bool False -> eval alt
    _          -> throwE $ TypeMismatch "boolean" pred'

eval (Pair [Symbol "defined?", Symbol id])    = Bool <$> isDefined id
eval (Pair [Symbol "define", Symbol id, val]) = eval val >>= defineVar id >> return val
eval (Pair [Symbol "define", id, _])          = throwE $ TypeMismatch "symbol" id
eval (Pair (Symbol "define":args))            = throwE $ NumArgs 2 args
eval (Pair [Symbol "set!", Symbol id, val])   = eval val >>= setVar id >> return val
eval (Pair [Symbol "set!", id, _])            = throwE $ TypeMismatch "symbol" id
eval (Pair (Symbol "set!":args))              = throwE $ NumArgs 2 args
eval (Symbol id)                              = getVar id >>= maybe (throwE $ UnboundVar id) return

-- should we ensure that there are no totally free variables (as in bound nowhere -- even outside of the function) before constructing a function?
eval (Pair [Symbol "lambda", Symbol param, body]) = return $ Func { param = VariadicParam param, body, closure = Map.empty }
eval (Pair [Symbol "lambda", Pair params, body]) = do
  paramNames <- lift $ mapM (returnInExcept . unpackSymbol) params
  return $ Func { param = NaryParam paramNames, body, closure = Map.empty }

-- TODO: implement let and function definition syntax sugar

eval (Pair (func:args)) = do
  func <- eval func
  args <- mapM eval args
  apply func args

eval val@(Pair _)     = return val
eval val@(Number _)   = return val
eval val@(String _)   = return val
eval val@(Bool _)     = return val
eval val@Func {}      = return val
eval val@(PrimFunc _) = return val

apply :: LispVal -> [LispVal] -> LispInterp LispVal
apply Func { param, body, closure } args = do
  pushScope closure
  paramToScope param args >>= pushScope
  ret <- eval body >>= returnFromScope
  popScope
  return ret
apply (PrimFunc f) args         = lift $ f args
apply func _                    = throwE $ TypeMismatch "function" func

paramToScope :: LispParam -> [LispVal] -> LispInterp LispScope
paramToScope (VariadicParam p) v = return $ Map.singleton p $ Pair v
paramToScope (NaryParam ps) vs   =
  if length ps == length vs
    then return $ Map.fromList $ zip ps vs
    else throwE $ NumArgs (toInteger $ length ps) vs

-- pop a scope from the stack, returning a value after modifying it for safe use outside of the popped scope
-- this allows functions to close over variables from a scope before they pass out of it
returnFromScope :: LispVal -> LispInterp LispVal
returnFromScope val = do
  scope <- popScope
  case val of
    -- NOTE: since we're using the lazy `Map`, it's probably fine to add the entire scope to the closure,
    --       but we could also filter it and add only the entries corresponding to free variables
    func @ Func { closure } -> return func { closure = Map.union closure scope }
    _                       -> return val

defineVar :: String -> LispVal -> LispInterp ()
defineVar id val = do
  env <- get
  let old = NE.head env
  let new = Map.insert id val old
  put $ new NE.:| NE.tail env

setVar :: String -> LispVal -> LispInterp ()
setVar id val = get >>= lift . returnInExcept . setVar' . NE.toList >>= put
  where
    setVar' :: [LispScope] -> LispExcept LispState
    setVar' [] = E.throwE $ UnboundVar id
    setVar' (scope:scopes) = if Map.member id scope
      then return $ Map.insert id val scope NE.:| scopes
      else setVar' scopes

getVar :: String -> LispInterp (Maybe LispVal)
getVar id = getFirst . foldMap (First . Map.lookup id) . (ambientScope NE.<|) <$> get

isDefined :: String -> LispInterp Bool
isDefined id = any (Map.member id) . (ambientScope NE.<|) <$> get

pushScope :: LispScope -> LispInterp ()
pushScope scope = modify $ NE.cons scope

pushEmptyScope :: LispInterp ()
pushEmptyScope = pushScope Map.empty

popScopePass :: a -> LispInterp a
popScopePass x = popScope >> return x

popScope :: LispInterp LispScope
popScope = do
  env <- get
  case env of
    _ NE.:| []     -> throwE $ Internal "popScope: empty environment"
    p NE.:| (s:ss) -> put (s NE.:| ss) >> return p

ambientScope :: LispScope
ambientScope = Map.fromList [
    ("symbol?", PrimFunc $ typeCheck isSymbol),
    ("pair?", PrimFunc $ typeCheck isPair),
    ("number?", PrimFunc $ typeCheck isNumber),
    ("string?", PrimFunc $ typeCheck isString),
    ("boolean?", PrimFunc $ typeCheck isBoolean),

    ("eqv?", PrimFunc $ equalityOp (==)),

    ("car", PrimFunc $ unaryOp unpackPairToCons id fst),
    ("cdr", PrimFunc $ unaryOp unpackPairToCons id snd),
    ("cons", PrimFunc consOp),

    ("not",  PrimFunc $ unaryOp unpackBool Bool not),
    ("&&",  PrimFunc $ variadicFoldingOp unpackBool Bool (&&) True),
    ("||",  PrimFunc $ variadicFoldingOp unpackBool Bool (||) False),

    ("+",  PrimFunc $ variadicFoldingOp unpackNumber Number (+) 0),
    ("-",  PrimFunc $ variadicFoldingOp unpackNumber Number (-) 0),
    ("*",  PrimFunc $ variadicFoldingOp unpackNumber Number (*) 1),
    ("/",  PrimFunc $ variadicFoldingOp unpackNumber Number div 1),
    ("modulo",  PrimFunc $ binaryOp unpackNumber Number mod),
    ("quotient",  PrimFunc $ binaryOp unpackNumber Number quot),
    ("remainder",  PrimFunc $ binaryOp unpackNumber Number rem),

    ("=",  PrimFunc $ binaryOp unpackNumber Bool (==)),
    ("<",  PrimFunc $ binaryOp unpackNumber Bool (<)),
    (">",  PrimFunc $ binaryOp unpackNumber Bool (>)),
    ("/=",  PrimFunc $ binaryOp unpackNumber Bool (/=)),
    (">=",  PrimFunc $ binaryOp unpackNumber Bool (>=)),
    ("<=",  PrimFunc $ binaryOp unpackNumber Bool (<=)),

    -- debugging functions
    ("print", PrimFunc $ fmap (const $ Pair []) . mapM (liftIO . print))
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
    unaryOp _ _ _ args       = E.throwE $ NumArgs 1 args

    binaryOp :: (LispVal -> LispExcept a) -> (r -> LispVal) -> (a -> a -> r) -> [LispVal] -> LispExceptT IO LispVal
    binaryOp unpack con f [a, b] = do
      a' <- returnInExcept $ unpack a
      b' <- returnInExcept $ unpack b
      return . con $ f a' b'
    binaryOp _ _ _ args = E.throwE $ NumArgs 2 args

    variadicFoldingOp :: (LispVal -> LispExcept a) -> (a -> LispVal) -> (a -> a -> a) -> a -> [LispVal] -> LispExceptT IO LispVal
    variadicFoldingOp unpack cons op identity params = return . cons $ foldl op identity $ map (fromRight identity . E.runExcept . unpack) params

    -- we can't use `binaryOp` for this because the unpacking is different for `car` and `cdr`
    consOp :: [LispVal] -> LispExceptT IO LispVal
    consOp [car, Pair cdr] = return $ Pair (car : cdr)
    consOp [_, cdr]        = E.throwE $ TypeMismatch "pair" cdr
    consOp args            = E.throwE $ NumArgs 2 args
