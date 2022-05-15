{-# LANGUAGE NamedFieldPuns #-}

module Vanessa.Interpret
  ( eval
  , LispScope
  , LispState
  , startState
  , LispInterp
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.State
import           Data.Either                (fromRight, rights)
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map.Lazy              as Map
import qualified Data.Monoid                as Monoid
import qualified Data.Set                   as Set
import           GHC.IO.IOMode              (IOMode (ReadMode, WriteMode))
import           System.IO                  (hClose, openFile)
import           Vanessa.Core
import           Vanessa.Value

startState :: LispState
startState = Map.empty NE.:| []

-- evaluate a lisp expression and execute its side effects
eval :: LispVal -> LispInterp LispVal
eval (Pair [Symbol "quote", val]) = return val
eval (Pair [Symbol "if", pred, conseq, alt]) = do
  pred' <- eval pred
  case pred' of
    Bool True  -> eval conseq
    Bool False -> eval alt
    _          -> throwE $ TypeMismatch "boolean" pred'
eval (Pair [Symbol "defined?", Symbol id]) = Bool <$> isDefined id
eval (Pair [Symbol "define", Symbol id, val]) =
  eval val >>= defineVar id >> return val
eval (Pair [Symbol "define", id, _]) = throwE $ TypeMismatch "symbol" id
eval (Pair (Symbol "define":args)) = throwE $ NumArgs 2 args
eval (Pair [Symbol "set!", Symbol id, val]) =
  eval val >>= setVar id >> return val
eval (Pair [Symbol "set!", id, _]) = throwE $ TypeMismatch "symbol" id
eval (Pair (Symbol "set!":args)) = throwE $ NumArgs 2 args
eval (Symbol id) = getVar id >>= maybe (throwE $ UnboundVar id) return
eval (Pair [Symbol "lambda", Symbol param, body]) =
  return $ Func {param = VariadicParam param, body, closure = Map.empty}
eval (Pair [Symbol "lambda", Pair params, body]) = do
  paramNames <- lift $ mapM (returnInExcept . unpackSymbol) params
  return $ Func {param = NaryParam paramNames, body, closure = Map.empty}
eval (Pair [Symbol "let", Pair bindings, body]) =
  pairsToBindings bindings >>= pushScope >> eval body >>= returnFromScope
eval (Pair (func:args)) = do
  func <- eval func
  args <- mapM eval args
  apply func args
eval val@(Pair _) = return val
eval val@(Number _) = return val
eval val@(String _) = return val
eval val@(Bool _) = return val
eval val@Func {} = return val
eval val@(PrimFunc _) = return val
eval val@(Port _) = return val

apply :: LispVal -> [LispVal] -> LispInterp LispVal
apply Func {param, body, closure} args = do
  pushScope closure
  paramToScope param args >>= pushScope
  ret <- eval body >>= returnFromScope
  popScope
  return ret
apply (PrimFunc f) args = f args
apply func _ = throwE $ TypeMismatch "function" func

pairsToBindings :: [LispVal] -> LispInterp LispScope
pairsToBindings [] = return Map.empty
pairsToBindings (Pair [Symbol id, val]:ps) = do
  val <- eval val
  rest <- pairsToBindings ps
  -- prefer the existing value because it is later in the binding list
  return $ Map.insertWith (\_ x -> x) id val rest
pairsToBindings ps =
  throwE $ TypeMismatch "list of binding pair lists" $ Pair ps

paramToScope :: LispParam -> [LispVal] -> LispInterp LispScope
paramToScope (VariadicParam p) v = return $ Map.singleton p $ Pair v
paramToScope (NaryParam ps) vs =
  if length ps == length vs
    then return $ Map.fromList $ zip ps vs
    else throwE $ NumArgs (toInteger $ length ps) vs

-- pop a scope from the stack, returning a value after modifying it for safe use outside of the popped scope
-- this allows functions to close over variables from a scope before they pass out of it
returnFromScope :: LispVal -> LispInterp LispVal
returnFromScope val = do
  scope <- popScope
  case val
    -- NOTE: since we're using the lazy `Map`, it's probably fine to add the entire scope to the closure,
    --       but we could also filter it and add only the entries corresponding to free variables
        of
    func@Func {closure} -> return func {closure = Map.union closure scope}
    _                   -> return val

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
    setVar' (scope:scopes) =
      if Map.member id scope
        then return $ Map.insert id val scope NE.:| scopes
        else setVar' scopes

getVar :: String -> LispInterp (Maybe LispVal)
getVar id =
  Monoid.getFirst .
  foldMap (Monoid.First . Map.lookup id) . (ambientScope NE.<|) <$>
  get

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
ambientScope =
  Map.fromList
    [ ("apply", PrimFunc applyOp)
    , ("symbol?", PrimFunc $ typeCheck isSymbol)
    , ("pair?", PrimFunc $ typeCheck isPair)
    , ("number?", PrimFunc $ typeCheck isNumber)
    , ("string?", PrimFunc $ typeCheck isString)
    , ("boolean?", PrimFunc $ typeCheck isBoolean)
    , ("eqv?", PrimFunc $ equalityOp (==))
    , ("car", PrimFunc $ unaryOp unpackPairToCons id fst)
    , ("cdr", PrimFunc $ unaryOp unpackPairToCons id snd)
    , ("cons", PrimFunc consOp)
    , ("not", PrimFunc $ unaryOp unpackBool Bool not)
    , ("&&", PrimFunc $ variadicFoldingOp unpackBool Bool (&&) True)
    , ("||", PrimFunc $ variadicFoldingOp unpackBool Bool (||) False)
    , ("+", PrimFunc $ variadicFoldingOp unpackNumber Number (+) 0)
    , ("-", PrimFunc $ variadicFoldingOp unpackNumber Number (-) 0)
    , ("*", PrimFunc $ variadicFoldingOp unpackNumber Number (*) 1)
    , ("/", PrimFunc $ variadicFoldingOp unpackNumber Number div 1)
    , ("modulo", PrimFunc $ binaryOp unpackNumber Number mod)
    , ("quotient", PrimFunc $ binaryOp unpackNumber Number quot)
    , ("remainder", PrimFunc $ binaryOp unpackNumber Number rem)
    , ("=", PrimFunc $ binaryOp unpackNumber Bool (==))
    , ("<", PrimFunc $ binaryOp unpackNumber Bool (<))
    , (">", PrimFunc $ binaryOp unpackNumber Bool (>))
    , ("/=", PrimFunc $ binaryOp unpackNumber Bool (/=))
    , (">=", PrimFunc $ binaryOp unpackNumber Bool (>=))
    , ("<=", PrimFunc $ binaryOp unpackNumber Bool (<=))
    , ("open-input-file", PrimFunc $ makePort ReadMode)
    , ("open-output-file", PrimFunc $ makePort WriteMode)
    , ("close-input-port", PrimFunc closePort)
    , ("close-output-port", PrimFunc closePort)
    -- , ("read", PrimFunc readProc)
    -- , ("write", PrimFunc writeProc)
    -- , ("read-contents", PrimFunc readContents)
    -- , ("read-all", PrimFunc readAll)
    ]

type BuiltinOp = [LispVal] -> LispInterp LispVal

applyOp :: BuiltinOp
applyOp []           = throwE $ NumArgs 1 []
applyOp [f, Pair as] = apply f as
applyOp [_, arg]     = throwE $ TypeMismatch "pair" arg
applyOp args         = throwE $ NumArgs 2 args

equalityOp ::
     (LispVal -> LispVal -> Bool) -> BuiltinOp
equalityOp f []     = return $ Bool True
equalityOp f (x:xs) = return . Bool $ all (f x) xs

typeCheck :: (LispVal -> Bool) -> BuiltinOp
typeCheck f args = return . Bool $ all f args

unaryOp ::
     (LispVal -> LispExcept a)
  -> (r -> LispVal)
  -> (a -> r)
  -> BuiltinOp
unaryOp unpack con f [a] = lift $ returnInExcept $ con . f <$> unpack a
unaryOp _ _ _ args       = throwE $ NumArgs 1 args

binaryOp ::
     (LispVal -> LispExcept a)
  -> (r -> LispVal)
  -> (a -> a -> r)
  -> BuiltinOp
binaryOp unpack con f [a, b] = do
  a' <- lift $ returnInExcept $ unpack a
  b' <- lift $ returnInExcept $ unpack b
  return . con $ f a' b'
binaryOp _ _ _ args = throwE $ NumArgs 2 args

variadicFoldingOp ::
     (LispVal -> LispExcept a)
  -> (a -> LispVal)
  -> (a -> a -> a)
  -> a
  -> BuiltinOp
variadicFoldingOp unpack cons op identity params =
  return . cons $
  foldl op identity $ map (fromRight identity . E.runExcept . unpack) params

-- we can't use `binaryOp` for this because the unpacking is different for `car` and `cdr`
consOp :: BuiltinOp
consOp [car, Pair cdr] = return $ Pair (car : cdr)
consOp [_, cdr]        = throwE $ TypeMismatch "pair" cdr
consOp args            = throwE $ NumArgs 2 args

makePort :: IOMode -> BuiltinOp
makePort mode [String filename] = liftIO $ Port <$> openFile filename mode
makePort _ [arg]                = throwE $ TypeMismatch "filename (string)" arg
makePort _ args                 = throwE $ NumArgs 1 args

closePort :: BuiltinOp
closePort [Port handle] = liftIO $ hClose handle >> return (Pair [])
closePort [arg]         = throwE $ TypeMismatch "port" arg
closePort args          = throwE $ NumArgs 1 args
