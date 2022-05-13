{-# LANGUAGE NamedFieldPuns #-}

module Vanessa.Debug where

import qualified Data.Map                       as Map
import           Text.PrettyPrint
import           Text.PrettyPrint.HughesPJClass
import           Vanessa.Core
import           Vanessa.Interpret              (LispState)

instance Pretty LispVal where
  pPrint (Symbol name) = text name
  pPrint (Pair contents) = parens $ hsep $ map pPrint contents
  pPrint (Number n) = integer n
  pPrint (String s) = doubleQuotes $ text s
  pPrint (Bool True) = text "#t"
  pPrint (Bool False) = text "#f"
  pPrint (PrimFunc _) = text "{primitive function}"
  pPrint Func {param, body, closure} =
    let combinator = parens $ hsep [text "lambda", pPrint param, pPrint body]
        closureBindings =
          parens $
          hsep $ map (\(id, val) -> text id <+> pPrint val) $ Map.toList closure
     in if Map.null closure
          then combinator
          else parens $ hsep [text "let", closureBindings, combinator]

instance Pretty LispParam where
  pPrint (NaryParam ps)    = pPrint $ Pair $ map Symbol ps
  pPrint (VariadicParam p) = pPrint $ Symbol p

instance Show LispError where
  show (ParseError message) = "Parse error: " ++ message
  show (NumArgs expected arguments) =
    "Expected " ++
    show expected ++ " arguments, but got " ++ show (length arguments)
  show (TypeMismatch expected actual) =
    "Expected " ++ expected ++ ", but got " ++ prettyShow actual
  show (UnboundVar name) = "Variable '" ++ name ++ "' is unbound"
  show (Internal message) = "Internal error: " ++ message

instance Pretty LispState