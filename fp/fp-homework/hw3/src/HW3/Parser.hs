module HW3.Parser
  ( parse
  ) where

import HW3.Base
import Control.Monad (void)
import Control.Monad.Combinators.Expr as E
import Data.Scientific (Scientific (base10Exponent, coefficient))
import Data.Char (digitToInt, isAlpha, isAlphaNum)
import Data.Functor (($>))
import Data.ByteString (pack)
import Data.List (intercalate)
import Data.Ratio ((%))
import Data.Word (Word8)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof, notFollowedBy, try), ParseErrorBundle, Parsec,
                        choice, many, manyTill, runParser, satisfy, sepBy, sepEndBy, some, (<|>))
import Text.Megaparsec.Char (char, hexDigitChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (do
                    skip
                    res <- parseExpr
                    skip
                    eof
                    return res) ""

parseExpr :: Parser HiExpr
parseExpr = E.makeExprParser (
      parseValue
  <|> (withParens parseExpr)
  <|> parseName
  <|> parseList
  <|> parseDict
  ) table

parseList :: Parser HiExpr
parseList = HiExprApply (HiExprValue $ HiValueFunction HiFunList) <$> try (with "[" "]" (parseExpr `sepBy` symbol ","))

parseName :: Parser HiExpr
parseName = HiExprValue <$> (HiValueAction <$> (HiActionCwd <$ symbol "cwd" <|> HiActionNow <$ symbol "now"))

table :: [[E.Operator Parser HiExpr]]
table = [ [ applyOrRun ]
          , [ infixL "*" $ applyBinary HiFunMul,
              E.InfixL $ applyBinary HiFunDiv <$ (lexeme . try) (string "/" <* notFollowedBy (char '='))]
          , [ infixL "+" $ applyBinary HiFunAdd, infixL "-" $ applyBinary HiFunSub ]
          , [ infixN "==" $ applyBinary HiFunEquals, infixN "/=" $ applyBinary HiFunNotEquals,
              infixN ">=" $ applyBinary HiFunNotLessThan, infixN "<=" $ applyBinary HiFunNotGreaterThan,
              infixN ">" $ applyBinary HiFunGreaterThan, infixN "<" $ applyBinary HiFunLessThan ]
          , [ infixR "&&" $ applyBinary HiFunAnd ]
          , [ infixR "||" $ applyBinary HiFunOr  ]
        ]
  where
    infixL, infixR, infixN :: String -> (HiExpr -> HiExpr -> HiExpr) -> E.Operator Parser HiExpr
    infixL name f = E.InfixL (f <$ symbol name)
    infixR name f = E.InfixR (f <$ symbol name)
    infixN name f = E.InfixN (f <$ symbol name)

    applyBinary :: HiFun -> HiExpr -> HiExpr -> HiExpr
    applyBinary fun a b = HiExprApply (HiExprValue (HiValueFunction fun)) [a, b]

applyOrRun :: E.Operator Parser HiExpr
applyOrRun = E.Postfix $ foldr1 (flip (.)) . map applyPar <$> parseArgumentsOrRun
    where
      applyPar :: (Maybe [HiExpr]) -> HiExpr -> HiExpr
      applyPar ma expr = case ma of
        Nothing     -> HiExprRun expr
        (Just args) -> HiExprApply expr args

parseArgumentsOrRun :: Parser [Maybe [HiExpr]]
parseArgumentsOrRun = some $
      (Just <$> withParens (parseExpr `sepBy` symbol ","))
  <|> (char '.' *> (Just . (: []) . HiExprValue . HiValueString . T.pack . intercalate "-" <$> parseArgs))
  <|> (Nothing <$ symbol "!")

parseArgs :: Parser [[Char]]
parseArgs = ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy` char '-'

parseValue :: Parser HiExpr
parseValue = HiExprValue <$>
  (   parseNumber
  <|> parseFunction
  <|> parseBoolean
  <|> parseString
  <|> parseNull
  <|> parseBytes )

with :: String -> String -> Parser a -> Parser a
with l r p = do
  _ <- (symbol l)
  res <- p
  _ <- (symbol r)
  return res

withParens :: Parser a -> Parser a
withParens = with "(" ")"

skip :: Parser ()
skip = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skip

symbol :: String -> Parser String
symbol = L.symbol skip

parseNull :: Parser HiValue
parseNull =  HiValueNull <$ symbol "null"

parseString :: Parser HiValue
parseString = HiValueString . T.pack <$> (string "\"" *> manyTill L.charLiteral (symbol "\""))

parseBoolean :: Parser HiValue
parseBoolean = HiValueBool <$> (symbol "true" $> True <|> symbol "false" $> False)

parseNumber :: Parser HiValue
parseNumber = HiValueNumber <$> (do
  sc <- lexeme $ L.signed skip L.scientific
  let e = base10Exponent sc
  let k = coefficient sc
  return (if e < 0
    then k % (10 ^ (-e))
    else (10 ^ e * k) % 1))

parseFunction :: Parser HiValue
parseFunction = HiValueFunction <$> (choice $ map (\f -> f <$ symbol (showFun f)) (enumFrom minBound :: [HiFun]))

parseBytes :: Parser HiValue
parseBytes = HiValueBytes <$> (pack <$> try (with "[" "]" (with "#" "#" (parseByte `sepEndBy` lexeme space1))))

parseByte :: Parser Word8
parseByte = do
  f <- digitToInt <$> hexDigitChar
  s <- digitToInt <$> hexDigitChar
  return $ fromIntegral (f * 16 + s)

parseDict :: Parser HiExpr
parseDict = HiExprDict <$> (with "{" "}" (element `sepBy` symbol ","))
  where
    element :: Parser (HiExpr, HiExpr)
    element = do
      _ <- skip
      key <- parseExpr
      _ <- skip
      void $ symbol ":"
      _ <- skip
      value <- parseExpr
      _ <- skip
      return (key, value)
