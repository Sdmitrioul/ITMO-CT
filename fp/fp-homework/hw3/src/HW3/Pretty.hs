{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module HW3.Pretty
  ( prettyValue
  ) where

import HW3.Base (HiAction (..), HiValue (..), showFun)
import qualified Data.ByteString as B
import qualified Data.Text as T (pack)
import qualified Data.Map as M
import Data.Ratio (denominator, numerator)
import Data.Scientific (FPFormat (..), formatScientific, fromRationalRepetendUnlimited)
import Prettyprinter (Doc, brackets, comma, concatWith, enclose, parens, pretty, space, surround, viaShow, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle)
import Text.Printf (printf)

prettyAction :: HiAction -> Doc AnsiStyle
prettyAction = \case
  HiActionCwd              -> "cwd"
  HiActionRead path        -> "read" <> parens (viaShow $ T.pack path)
  HiActionWrite path bytes -> "write" <> parens (viaShow (T.pack path) <> comma <+> prettyValue (HiValueBytes bytes))
  HiActionMkDir path       -> "mkdir" <> parens (viaShow $ T.pack path)
  HiActionChDir path       -> "cd" <> parens (viaShow $ T.pack path)
  HiActionNow              -> "now"
  HiActionRand i j         -> "rand" <> parens (pretty i <> comma <+> pretty j)
  HiActionEcho text        -> "echo" <> parens (viaShow text)

prettyRational :: Integer -> Integer -> Doc AnsiStyle
prettyRational n d = pretty n <> "/" <> pretty d

prettyNum :: Rational -> Doc AnsiStyle
prettyNum rat = case fromRationalRepetendUnlimited rat of
  (sc, Nothing) -> case quotRem (numerator rat) (denominator rat) of
    (n, 0) -> pretty n
    _      -> pretty (formatScientific Fixed Nothing sc)
  (_, Just _) -> case quotRem (numerator rat) (denominator rat) of
    (n, 0) -> pretty n
    (0, d) -> prettyRational d $ denominator rat
    (n, d) -> if d > 0
      then pretty n <+> "+" <+> prettyRational d (denominator rat)
      else pretty n <+> "-" <+> prettyRational (-d) (denominator rat)

prettyPair :: (HiValue, HiValue) -> Doc AnsiStyle
prettyPair (k, v) = prettyValue k <> ":" <+> prettyValue v

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = \case
  HiValueNumber n    -> prettyNum n
  HiValueFunction fn -> pretty $ showFun fn
  HiValueBool True   -> "true"
  HiValueBool False  -> "false"
  HiValueNull        -> "null"
  HiValueString text -> viaShow text
  HiValueList list   -> brackets $ concatWith (surround ",") (fmap ((space <>) . prettyValue) list) <> space
  HiValueBytes b     -> enclose "[# " " #]" $ pretty $ unwords (map (printf "%02x") $ B.unpack b)
  HiValueDict dict   -> enclose "{ " " }" $ concatWith (surround ", ") (map prettyPair $ M.toList dict) <> space
  HiValueTime time   -> pretty $ "parse-time(\"" ++ show time ++ "\")"
  HiValueAction a    -> prettyAction a
 