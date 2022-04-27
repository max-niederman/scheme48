module Main where

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
isPair (List _)       = True
isPair (DottedList _ _) = True
isPair _              = False

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

builtinFunctions :: [(String, [LispVal] -> LispVal)]
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

    equalityOp :: (LispVal -> LispVal -> Bool) -> [LispVal] -> LispVal 
    equalityOp f [] = Bool True
    equalityOp f (x:xs) = Bool $ all (f x) xs

    allBoolean :: (LispVal -> Bool) -> [LispVal] -> LispVal
    allBoolean f args = Bool $ all f args

    numericBinaryOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
    numericBinaryOp op [Number a, Number b] = Number $ op a b
    numericBinaryOp op _                    = Number 0

    numericVariadicOp :: (Integer -> Integer -> Integer) -> Integer -> [LispVal] -> LispVal
    numericVariadicOp op identity params = Number $ foldl op identity $ map (fromMaybe 0 . unpackNumber) params

eval :: LispVal -> LispVal
eval (List [Symbol "quote", val]) = val
eval val@(String _)             = val
eval val@(Number _)             = val
eval val@(Bool _)               = val
eval (List (Symbol func : args))  = apply func $ map eval args

-- TODO: improve error handling
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Symbol "error") ($ args) $ lookup func builtinFunctions

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> String $ "Error:\n" ++ show err
  Right val -> val

main :: IO ()
main = head <$> getArgs >>= readFile >>= print . eval . readExpr
