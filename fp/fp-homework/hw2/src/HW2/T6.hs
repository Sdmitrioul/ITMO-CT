{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module HW2.T6
  (
    ParseError (..)
  , Parser (..)
  , pChar
  , pEof
  , parseError
  , runP
  , parseExpr
  ) where

import Control.Applicative (Alternative (..), Applicative (..), optional)
import Control.Monad (Monad, Functor, MonadPlus, mfilter, msum, void)
import GHC.Natural
import Data.Char
import Data.Scientific (scientific, toRealFloat)
import HW2.T1 (Annotated (..), Except (..))
import HW2.T4 (Expr (..), Prim (..))
import HW2.T5 (ExceptState (..))

newtype ParseError = ErrorAtPos Natural deriving (Show)

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P parser) input =
  case runES parser (0, input) of
    Error e          -> Error e
    Success (a :# _) -> Success a

-- when input is empty parser return Error with position parameter, 
-- if input not empty, parser return readed character anotated with state, 
-- that was provided, but counter of readed positions is incremented and tail of input without readed char.
pChar :: Parser Char
pChar = P $ ES (\(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs)))

parseError :: Parser a
parseError = P $ ES (\(pos,_) -> Error (ErrorAtPos pos))

parseOr :: Parser a -> Parser a -> Parser a
parseOr (P f) (P sec) = P $ ES (\s ->
    case runES f s of
      (Error _) -> runES sec s
      x         -> x
      )


instance Alternative Parser where
  empty = parseError
  (<|>) = parseOr

instance MonadPlus Parser

pEof :: Parser ()
pEof = P $ ES (\(pos, s) ->
  case s of
    []     -> Success (() :# (pos, s))
    (c:cs) -> Error (ErrorAtPos pos))

pAbbr :: Parser String
pAbbr = do
  abbr <- some (mfilter Data.Char.isUpper pChar)
  pEof
  pure abbr

parseExpr :: String -> Except ParseError Expr
parseExpr = runP exprParser

exprParser :: Parser Expr
exprParser = do
           val <- parse
           skipSpaces
           pEof
           return val

parse :: Parser Expr
parse = parseLeft "+-" (parseLeft "*/" parseTerm)

parseLeft :: String -> Parser Expr -> Parser Expr
parseLeft el p = do
  skipSpaces
  res <- p
  parseLeftRec res el p

parseLeftRec :: Expr -> String -> Parser Expr -> Parser Expr
parseLeftRec acc el p = do
  op <- optional (parseOp el)
  case op of
    Nothing -> return acc
    (Just x) -> do
      skipSpaces
      next <- p
      parseLeftRec (Op (x acc next)) el p

parseOp :: String -> Parser (Expr -> Expr -> Prim Expr)
parseOp el = do
  skipSpaces
  op <- mfilter (`elem` el) pChar
  return (charToOp op)

parseDigits :: Parser String
parseDigits = some (mfilter Data.Char.isDigit pChar)

strToInteger :: String -> Integer
strToInteger str = fromIntegral (foldl (\a x -> a * 10 + x) 0 (map charToDigit str))

parseDouble :: Parser Expr
parseDouble = do
                firstDigits <- parseDigits
                _ <- optional (charParser '.')
                nextDigits <- optional parseDigits
                case nextDigits of
                  (Just x) -> do
                    let intValue = strToInteger (firstDigits ++ x)
                    let expLen = length x
                    return (Val (toRealFloat (scientific intValue (-expLen))))
                  _ -> return (Val (fromInteger (strToInteger firstDigits)))

parseBrackets :: Parser Expr
parseBrackets = do
          skipSpaces
          _ <- charParser '('
          res <- parse
          skipSpaces
          _ <- charParser ')'
          return res

parseTerm :: Parser Expr
parseTerm = do
  skipSpaces
  num <- optional parseDouble
  case num of
    (Just n) -> return n
    _        -> do
      br <- parseBrackets
      return br

charParser :: Char -> Parser Char
charParser ch = mfilter (== ch) pChar

skipSpaces :: Parser ()
skipSpaces = void (many (charParser ' '))

charToOp :: Char -> (Expr -> Expr -> Prim Expr)
charToOp '+' = Add
charToOp '-' = Sub
charToOp '*' = Mul
charToOp '/' = Div
charToOp x   = error ("unexpected operation char" ++ [x])

charToDigit :: Char -> Int
charToDigit '0' = 0
charToDigit '1' = 1
charToDigit '2' = 2
charToDigit '3' = 3
charToDigit '4' = 4
charToDigit '5' = 5
charToDigit '6' = 6
charToDigit '7' = 7
charToDigit '8' = 8
charToDigit '9' = 9
charToDigit  x  = error ("unexpected operation char" ++ [x])
