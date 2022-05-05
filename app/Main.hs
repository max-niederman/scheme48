module Main where

import           Control.Monad
import qualified Data.Bifunctor                as Bifunctor
import           Data.Either                   (fromRight)
import           Numeric                       (readHex, readOct)
import           System.Environment            (getArgs)
import           Text.ParserCombinators.Parsec

whitespace :: Parser ()
whitespace = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

data LispVal
  = Symbol String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  deriving (Eq)

isSymbol :: LispVal -> Bool
isSymbol (Symbol _) = True
isSymbol _          = False

isPair :: LispVal -> Bool
isPair (List _)         = True
isPair (DottedList _ _) = True
isPair _                = False

isNumber :: LispVal -> Bool
isNumber (Number _) = True
isNumber _          = False

isString :: LispVal -> Bool
isString (String _) = True
isString _          = False

isBoolean :: LispVal -> Bool
isBoolean (Bool _) = True
isBoolean _        = False

unpackNumber :: LispVal -> Fallible Integer
unpackNumber (Number n) = return n
unpackNumber val        = Left $ TypeMismatch "number" val

unpackBool :: LispVal -> Fallible Bool
unpackBool (Bool b) = return b
unpackBool val        = Left $ TypeMismatch "boolean" val

instance Show LispVal where
  show (Symbol name) = name
  show (List contents) = "(" ++ unwords (map show contents) ++ ")"
  show (DottedList proper tail) = "(" ++ unwords (map show proper) ++ " . " ++ show tail ++ ")"
  show (Number n) = show n
  show (String s) = "\"" ++ s ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"

data LispError
  = ParseError String
  | NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | UnboundVar String

type Fallible = Either LispError

instance Show LispError where
  show (ParseError message) = "Parse error: " ++ message
  show (NumArgs expected arguments) =
    "Expected " ++ show expected ++ " arguments, but got " ++ show (length arguments)
  show (TypeMismatch expected actual) =
    "Expected " ++ expected ++ ", but got " ++ show actual
  show (UnboundVar name) =
    "Variable '" ++ name ++ "' is unbound"

parseExpr :: Parser LispVal
parseExpr = choice [parseQuoted, parseList, parseString, parseNumber, parseSymbol]

parseSymbol :: Parser LispVal
parseSymbol = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let symbol = first : rest
  return $ case symbol of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Symbol symbol

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  expr <- parseExpr
  return $ List [Symbol "quote", expr]

parseList :: Parser LispVal
parseList = do
  char '('
  proper <- sepEndBy parseExpr whitespace
  list <- DottedList proper <$> (char '.' >> spaces >> parseExpr) <|> return (List proper)
  char ')'
  return list

--   TODO: implement the rest of the R5RS number spec i.e. floating points, binary notation, complex numbers
parseNumber :: Parser LispVal
parseNumber =
  try parseEscaped <|> parseDecimal
  where
    parseDecimal :: Parser LispVal
    parseDecimal = Number . read <$> many1 digit

    parseEscaped :: Parser LispVal
    parseEscaped =
      char '#'
        >> choice
          [ char 'o' >> Number . fst . head . readOct <$> many1 octDigit,
            char 'd' >> parseDecimal,
            char 'x' >> Number . fst . head . readHex <$> many1 hexDigit
          ]

parseString :: Parser LispVal
parseString = do
  char '"'
  contents <- many (parseEscape <|> noneOf ['"'])
  char '"'
  return $ String contents
  where
    parseEscape :: Parser Char
    parseEscape = do
      char '\\'
      code <- oneOf ['\\', '"', 'n', 'r', 't']
      return $ case code of
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        _   -> code

builtinFunctions :: [(String, [LispVal] -> Fallible LispVal)]
builtinFunctions = [
    ("symbol?", typeCheck isSymbol),
    ("pair?", typeCheck isPair),
    ("number?", typeCheck isNumber),
    ("string?", typeCheck isString),
    ("boolean?", typeCheck isBoolean),

    ("eqv?", equalityOp (==)),

    ("+",  variadicOp unpackNumber Number (+) 0),
    ("-",  variadicOp unpackNumber Number (-) 0),
    ("*",  variadicOp unpackNumber Number (*) 1),
    ("/",  variadicOp unpackNumber Number div 1),
    ("modulo",  binaryOp unpackNumber Number mod),
    ("quotient",  binaryOp unpackNumber Number quot),
    ("remainder",  binaryOp unpackNumber Number rem),

    ("=",  binaryOp unpackNumber Bool (==)),
    ("<",  binaryOp unpackNumber Bool (<)),
    (">",  binaryOp unpackNumber Bool (>)),
    ("/=",  binaryOp unpackNumber Bool (/=)),
    (">=",  binaryOp unpackNumber Bool (>=)),
    ("<=",  binaryOp unpackNumber Bool (<=)),

    ("&&",  variadicOp unpackBool Bool (&&) True),
    ("||",  variadicOp unpackBool Bool (||) False)
  ]

  where
    -- TODO: improve error handling

    equalityOp :: (LispVal -> LispVal -> Bool) -> [LispVal] -> Fallible LispVal
    equalityOp f []     = return $ Bool True
    equalityOp f (x:xs) = return . Bool $ all (f x) xs

    typeCheck :: (LispVal -> Bool) -> [LispVal] -> Fallible LispVal
    typeCheck f args = return . Bool $ all f args

    binaryOp :: (LispVal -> Fallible a) -> (r -> LispVal) -> (a -> a -> r) -> [LispVal] -> Fallible LispVal
    binaryOp ext cons f [a, b] = do
      a' <- ext a
      b' <- ext b
      return . cons $ f a' b'
    binaryOp _ _ _ args = Left $ NumArgs 2 args

    variadicOp :: (LispVal -> Fallible a) -> (a -> LispVal) -> (a -> a -> a) -> a -> [LispVal] -> Fallible LispVal
    variadicOp ext cons op identity params = return . cons $ foldl op identity $ map (fromRight identity . ext) params

eval :: LispVal -> Fallible LispVal
eval (List [Symbol "quote", val]) = return val
eval val@(Symbol _)               = return val
eval val@(Number _)               = return val
eval val@(String _)               = return val
eval val@(Bool _)                 = return val
eval (List (Symbol func : args))  = mapM eval args >>= apply func

-- TODO: improve error handling
apply :: String -> [LispVal] -> Fallible LispVal
apply name args = case lookup name builtinFunctions of
  Just f  -> f args
  Nothing -> Left $ UnboundVar name

readExpr :: String -> Fallible LispVal
readExpr input = Bifunctor.first (ParseError . show) $ parse parseExpr "lisp" input

main :: IO ()
main = getArgs >>= sourceFromArgs >>= either (putStrLn . ("Error:\n" ++) . show) print . (eval <=< readExpr)
  where
    sourceFromArgs :: [String] -> IO String
    sourceFromArgs []    = readFile "/dev/stdin"
    sourceFromArgs (x:_) = readFile x
