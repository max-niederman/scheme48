module Lisp.Parse
  ( parseLisp
  , parseLispMany
  , parseExpr
  ) where

import           Control.Monad.Trans.Except
import qualified Data.Bifunctor                as Bifunctor
import           Lisp.Core
import           Numeric                       (readHex, readOct)
import           Text.ParserCombinators.Parsec

parseLisp :: Monad m => String -> LispExceptT m LispVal
parseLisp =
  returnInExcept .
  except .
  Bifunctor.first (ParseError . show) . parse parseExpr "lisp" . stripComments

parseLispMany :: Monad m => String -> LispExceptT m [LispVal]
parseLispMany =
  returnInExcept .
  except .
  Bifunctor.first (ParseError . show) .
  parse (sepEndBy parseExpr $ many1 newline) "lisp" . stripComments

-- NOTE: there may be a nice way to do this using Parsec idk it
stripComments :: String -> String
stripComments (';':cs) = stripComments $ tail $ dropWhile (/= '\n') cs
stripComments (c:cs)   = c : stripComments cs
stripComments []       = []

parseExpr :: Parser LispVal
parseExpr =
  choice [parseQuoted, parsePair, parseString, parseNumber, parseSymbol]

parseSymbol :: Parser LispVal
parseSymbol = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let symbol = first : rest
  return $
    case symbol of
      "#t" -> Bool True
      "#f" -> Bool False
      _    -> Symbol symbol

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  expr <- parseExpr
  return $ Pair [Symbol "quote", expr]

parsePair :: Parser LispVal
parsePair = do
  char '('
  elements <- sepEndBy parseExpr whitespace
  char ')'
  return $ Pair elements

--   TODO: implement the rest of the R5RS number spec i.e. floating points, binary notation, complex numbers
parseNumber :: Parser LispVal
parseNumber = try parseEscaped <|> parseDecimal
  where
    parseDecimal :: Parser LispVal
    parseDecimal = Number . read <$> many1 digit
    parseEscaped :: Parser LispVal
    parseEscaped =
      char '#' >>
      choice
        [ char 'o' >> Number . fst . head . readOct <$> many1 octDigit
        , char 'd' >> parseDecimal
        , char 'x' >> Number . fst . head . readHex <$> many1 hexDigit
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
      return $
        case code of
          'n' -> '\n'
          'r' -> '\r'
          't' -> '\t'
          _   -> code

whitespace :: Parser ()
whitespace = skipMany1 $ space <|> newline

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"
