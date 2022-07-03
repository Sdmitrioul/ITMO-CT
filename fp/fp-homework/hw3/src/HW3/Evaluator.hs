{-# LANGUAGE LambdaCase #-}

module HW3.Evaluator where

import Codec.Serialise
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import qualified Data.ByteString as B (pack, unpack)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Foldable (toList)
import qualified Data.Map as M (Map, elems, fromList, keys, lookup)
import Data.Semigroup (stimes)
import Data.Sequence (Seq, fromList)
import qualified Data.Text as T (Text, pack, singleton, strip, toLower, toUpper, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (addUTCTime, diffUTCTime)
import HW3.Base
import HW3.Helpers
import Prelude hiding (reverse)

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . evalHiExpr

evalHiExpr :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalHiExpr = \case
  (HiExprValue value)    -> return value
  (HiExprRun expr)       -> evalHiExpr expr >>= 
    \case
      (HiValueAction act) -> lift $ runAction act
      _                   -> throwE HiErrorInvalidArgument
  (HiExprDict dict)      -> HiValueDict . M.fromList <$> mapM (\(key, value) -> do
    keyRes <- evalHiExpr key
    valueRes <- evalHiExpr value
    return (keyRes, valueRes)) dict
  (HiExprApply fun args) -> evalApply fun args

evalApply :: HiMonad m => HiExpr -> [HiExpr] -> ExceptT HiError m HiValue
evalApply fun args = do
  funEv <- evalHiExpr fun
  case funEv of
    (HiValueFunction f) -> evalFun f args
    _                   -> evalQFun funEv args

evalQFun :: HiMonad m => HiValue -> [HiExpr] -> ExceptT HiError m HiValue
evalQFun val args = do
  argsEvl <- mapM evalHiExpr args
  case val of
    (HiValueString s) -> sliceString s argsEvl
    (HiValueList l)   -> sliceList l argsEvl
    (HiValueBytes b)  -> sliceBytes b argsEvl
    (HiValueDict d)   -> findInDict d argsEvl
    _                 -> throwE HiErrorInvalidFunction

sliceString :: HiMonad m => T.Text -> [HiValue] -> ExceptT HiError m HiValue
sliceString str [ind] = findByIndex (T.unpack str) ind (HiValueString . T.pack . (:[]))
sliceString str pos   = sliceElements (T.unpack str) pos (HiValueString . T.pack)

sliceList :: HiMonad m => Seq HiValue -> [HiValue] -> ExceptT HiError m HiValue
sliceList list [ind] = findByIndex list ind id
sliceList list pos   = sliceElements list pos HiValueList

sliceBytes :: HiMonad m => ByteString -> [HiValue] -> ExceptT HiError m HiValue
sliceBytes byt [ind] = findByIndex (B.unpack byt) ind (HiValueNumber . fromIntegral)
sliceBytes byt pos   = sliceElements (B.unpack byt) pos (HiValueBytes . B.pack)

findInDict :: HiMonad m => M.Map HiValue HiValue -> [HiValue] -> ExceptT HiError m HiValue
findInDict dict [key] = case M.lookup key dict of
  (Just val) -> return val
  Nothing    -> return HiValueNull
findInDict _ _        = throwE HiErrorArityMismatch

evalFun :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalFun fun args = case fun of
  HiFunAnd  -> evalHiLazy fun args
  HiFunOr   -> evalHiLazy fun args
  HiFunIf   -> evalHiLazy fun args
  HiFunList -> HiValueList . fromList <$> mapM evalHiExpr args
  _         -> evalHiStrict fun args

evalHiLazy :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalHiLazy HiFunIf [cond, pos, neg] = do
  calcCond <- evalHiExpr cond
  case calcCond of
    (HiValueBool c) -> if c then evalHiExpr pos else evalHiExpr neg
    _               -> throwE HiErrorInvalidArgument
evalHiLazy HiFunIf _ = throwE HiErrorArityMismatch
evalHiLazy fun [left, right] = do
  calcLeft <- evalHiExpr left
  case (fun, calcLeft) of
    (HiFunAnd, HiValueNull)       -> return HiValueNull
    (HiFunAnd, HiValueBool False) -> return calcLeft
    (HiFunAnd, _)                 -> evalHiExpr right
    (HiFunOr, HiValueNull)        -> evalHiExpr right
    (HiFunOr, HiValueBool False)  -> evalHiExpr right
    (HiFunOr, _)                  -> return calcLeft
    _                             -> throwE HiErrorInvalidFunction
evalHiLazy _ _       = throwE HiErrorArityMismatch

evalHiStrict :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalHiStrict f args = if isBinary f then evalHiBinary f args else evalHiUnary f args

evalHiUnary :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalHiUnary f [el] = do
  arg <- evalHiExpr el
  case (f, arg) of
    (HiFunNot, HiValueBool b)          -> return $ HiValueBool $ not b
    (HiFunLength, HiValueString s)     -> lengthOfHi s
    (HiFunLength, HiValueList l)       -> lengthOfHi l
    (HiFunLength, HiValueBytes b)      -> lengthOfHi b
    (HiFunToUpper, HiValueString s)    -> return $ HiValueString $ T.toUpper s
    (HiFunToLower, HiValueString s)    -> return $ HiValueString $ T.toLower s
    (HiFunReverse, HiValueString s)    -> reverseOfHi (\x -> HiValueString x) s
    (HiFunReverse, HiValueList l)      -> reverseOfHi (\x -> HiValueList x) l
    (HiFunReverse, HiValueBytes b)     -> reverseOfHi (\x -> HiValueBytes x) b
    (HiFunTrim, HiValueString s)       -> return $ HiValueString $ T.strip s
    (HiFunPackBytes, HiValueList l)    -> packHiBytes l
    (HiFunUnpackBytes, HiValueBytes b) -> unpackHiBytes b
    (HiFunEncodeUtf8, HiValueString s) -> return $ HiValueBytes $ encodeUtf8 s
    (HiFunDecodeUtf8, HiValueBytes b)  -> decodeHiBytes b
    (HiFunZip, HiValueBytes b)         -> zipHiBytes b
    (HiFunUnzip, HiValueBytes b)       -> unzipHiBytes b
    (HiFunSerialise, x)                -> return $ HiValueBytes $ toStrict $ serialise x
    (HiFunDeserialise, HiValueBytes b) -> return $ deserialise $ fromStrict b
    (HiFunRead, HiValueString path)    -> createHiAction HiActionRead path
    (HiFunMkDir, HiValueString path)   -> createHiAction HiActionMkDir path
    (HiFunChDir, HiValueString path)   -> createHiAction HiActionChDir path
    (HiFunParseTime, HiValueString s)  -> parseHiTime s
    (HiFunEcho, HiValueString s)       -> return $ HiValueAction $ HiActionEcho s
    (HiFunCount, HiValueString s)      -> countHiElements $ map (HiValueString . T.singleton) $ T.unpack s
    (HiFunCount, HiValueList l)        -> countHiElements $ toList l
    (HiFunCount, HiValueBytes b)       -> countHiElements $ map (HiValueNumber . fromIntegral) $ B.unpack b
    (HiFunKeys, HiValueDict d)         -> return $ HiValueList $ fromList $ M.keys d
    (HiFunValues, HiValueDict d)       -> return $ HiValueList $ fromList $ M.elems d
    (HiFunInvert, HiValueDict d)       -> invertHiDict d
    _                                  -> throwE HiErrorInvalidArgument
evalHiUnary _ _    = throwE HiErrorArityMismatch

evalHiBinary :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalHiBinary fn [leftR, rightR] = do
  left <- evalHiExpr leftR
  right <- evalHiExpr rightR
  case (fn, left, right) of
    (HiFunAdd, HiValueNumber x, HiValueNumber y)   -> return $ HiValueNumber $ x + y
    (HiFunAdd, HiValueString x, HiValueString y)   -> return $ HiValueString $ x <> y
    (HiFunAdd, HiValueList x, HiValueList y)       -> return $ HiValueList $ x <> y
    (HiFunAdd, HiValueBytes x, HiValueBytes y)     -> return $ HiValueBytes $ x <> y
    (HiFunAdd, HiValueTime t, HiValueNumber num)   -> return $ HiValueTime $ addUTCTime (fromRational num) t
    (HiFunAdd, HiValueNumber num, HiValueTime t)   -> return $ HiValueTime $ addUTCTime (fromRational num) t
    (HiFunSub, HiValueNumber x, HiValueNumber y)   -> return $ HiValueNumber $ x - y
    (HiFunSub, HiValueTime f, HiValueTime s)       -> return $ HiValueNumber $ toRational $ diffUTCTime f s
    (HiFunMul, HiValueNumber x, HiValueNumber y)   -> return $ HiValueNumber $ x * y
    (HiFunMul, HiValueString s, HiValueNumber n)   -> nHiTimes HiValueString s n
    (HiFunMul, HiValueNumber n, HiValueString s)   -> nHiTimes HiValueString s n
    (HiFunMul, HiValueList l, HiValueNumber n)     -> nHiTimes HiValueList l n
    (HiFunMul, HiValueNumber n, HiValueList l)     -> nHiTimes HiValueList l n
    (HiFunMul, HiValueBytes b, HiValueNumber n)    -> nHiTimes HiValueBytes b n
    (HiFunMul, HiValueNumber n, HiValueBytes b)    -> nHiTimes HiValueBytes b n
    (HiFunDiv, HiValueNumber _, HiValueNumber 0)   -> throwE HiErrorDivideByZero
    (HiFunDiv, HiValueNumber x, HiValueNumber y)   -> return $ HiValueNumber $ x / y
    (HiFunDiv, HiValueString fs, HiValueString ss) -> return $ HiValueString $ fs <> T.pack "/" <> ss
    (HiFunLessThan, x, y)                          -> return $ HiValueBool $ x < y
    (HiFunGreaterThan, x, y)                       -> return $ HiValueBool $ x > y
    (HiFunEquals, x, y)                            -> return $ HiValueBool $ x == y
    (HiFunNotLessThan, _, _)                       -> reverseCMP HiFunLessThan [leftR, rightR]
    (HiFunNotGreaterThan, _, _)                    -> reverseCMP HiFunGreaterThan [leftR, rightR]
    (HiFunNotEquals, _, _)                         -> reverseCMP HiFunEquals [leftR, rightR]
    (HiFunRange, HiValueNumber l, HiValueNumber r) -> return $ HiValueList $ fromList $ map HiValueNumber [l .. r]
    (HiFunFold, HiValueFunction f, HiValueList l)  -> evalHiFold f l
    (HiFunWrite, HiValueString p, HiValueString o) -> return $ HiValueAction $ HiActionWrite (T.unpack p) (encodeUtf8 o)
    (HiFunWrite, HiValueString p, HiValueBytes b)  -> return $ HiValueAction $ HiActionWrite (T.unpack p) b
    (HiFunRand, HiValueNumber l, HiValueNumber r)  -> randHiNumber l r
    _                                              -> throwE HiErrorInvalidArgument
evalHiBinary _ _ = throwE HiErrorArityMismatch

reverseCMP :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
reverseCMP f args = evalHiBinary f args >>= \case
  (HiValueBool res) -> return $ HiValueBool $ not res
  _                 -> throwE HiErrorInvalidArgument

randHiNumber :: HiMonad m => Rational -> Rational -> ExceptT HiError m HiValue
randHiNumber l r = do
  left <- checkAndGetInt l
  right <- checkAndGetInt r
  return $ HiValueAction $ HiActionRand (fromInteger left) (fromInteger right)

nHiTimes :: (HiMonad m, Semigroup e) => (e -> HiValue) -> e -> Rational -> ExceptT HiError m HiValue
nHiTimes f el num = do
  counts <- checkAndGetInt num
  return $ f $ stimes counts el

evalHiFold :: HiMonad m => HiFun -> Seq HiValue -> ExceptT HiError m HiValue
evalHiFold f col = if not $ isBinary f
  then throwE HiErrorInvalidArgument
  else if null col
    then return HiValueNull
    else foldl1 (\a b -> do
      ax <- a
      bx <- b
      evalFun f [HiExprValue ax, HiExprValue bx]) $ map pure $ toList col
