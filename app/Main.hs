module Main where

import           Data.Bifunctor
import           Control.Monad
import           Data.Maybe
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

unpackNumber :: LispVal -> Maybe Integer
unpackNumber (Number n) = Just n
unpackNumber _          = Nothing

instance Show LispVal where
  show (Symbol name) = name
  show (List contents) = "(" ++ (unwords $ map show contents) ++ ")"
  show (DottedList proper tail) = "(" ++ (unwords $ map show proper) ++ " . " ++ show tail ++ ")"
  show (Number n) = show n
  show (String s) = "\"" ++ s ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"

data LispError
  = ParseError String
  | NumArgs String [LispVal]
  | TypeMismatch String LispVal
  | UnboundVar String String

type Fallible = Either LispError

instance Show LispError where
  show (ParseError message) = "Parse error: " ++ message
  show (NumArgs expected arguments) =
    "Expected " ++ show expected ++ " arguments, but got " ++ show (length arguments)
  show (TypeMismatch expected actual) =
    "Expected " ++ expected ++ ", but got " ++ show actual
  show (UnboundVar varname) =
    "Variable '" ++ varname ++ "' is unbound"

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
    ("symbol?", allBoolean isSymbol),
    ("pair?", allBoolean isPair),
    ("number?", allBoolean isNumber),
    ("string?", allBoolean isString),
    ("boolean?", allBoolean isBoolean),

    ("eqv?", equalityOp (==)),

    ("+",  numericVariadicOp (+) 0),
    ("-",  numericVariadicOp (-) 0),
    ("*",  numericVariadicOp (*) 1),
    ("/",  numericVariadicOp div 1),
    ("modulo",  numericBinaryOp mod),
    ("quotient",  numericBinaryOp quot),
    ("remainder",  numericBinaryOp rem)
  ]
  where
    -- TODO: improve error handling

    equalityOp :: (LispVal -> LispVal -> Bool) -> [LispVal] -> Fallible LispVal
    equalityOp f []     = return . Bool True
    equalityOp f (x:xs) = return . Bool $ all (f x) xs

    allBoolean :: (LispVal -> Bool) -> [LispVal] -> Fallible LispVal
    allBoolean f args = return . Bool $ all f args

    numericBinaryOp :: (Integer -> Integer -> Integer) -> [LispVal] -> Fallible LispVal
    numericBinaryOp op [Number a, Number b] = return . Number $ op a b
    numericBinaryOp op _                    = Left $ NumArgs 2 []

    numericVariadicOp :: (Integer -> Integer -> Integer) -> Integer -> [LispVal] -> Fallible LispVal
    numericVariadicOp op identity params = return . Number $ foldl op identity $ map (fromMaybe 0 . unpackNumber) params

eval :: LispVal -> Fallible LispVal
eval (List [Symbol "quote", val]) = return val
eval val@(String _)               = return val
eval val@(Number _)               = return val
eval val@(Bool _)                 = return val
eval (List (Symbol func : args))  = apply func $ map eval args

-- TODO: improve error handling
apply :: String -> [LispVal] -> Fallible LispVal
apply func args = lookup func builtinFunctions args

readExpr :: String -> Fallible LispVal
readExpr input = bimap (ParseError . show) id $ parse parseExpr "lisp" input 

main :: IO ()
main = head <$> getArgs >>= readFile >>= print . eval . readExpr
