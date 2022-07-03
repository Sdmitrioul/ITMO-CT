{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase    #-}

module HW3.Base where

import Data.Map              (Map)
import Data.Sequence         (Seq)
import Data.Text             (Text)
import Data.Time             (UTCTime)
import Data.ByteString       (ByteString)
import Data.Data             (Typeable)
import GHC.Generics          (Generic)
import Codec.Serialise.Class (Serialise)

data HiFun
  = HiFunAdd
  | HiFunSub
  | HiFunAnd
  | HiFunDiv
  | HiFunEquals
  | HiFunGreaterThan
  | HiFunIf
  | HiFunLessThan
  | HiFunMul
  | HiFunNotEquals
  | HiFunNotGreaterThan
  | HiFunNotLessThan
  | HiFunNot
  | HiFunOr
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass Serialise

data HiValue
  = HiValueNull
  | HiValueBool Bool
  | HiValueFunction HiFun
  | HiValueNumber Rational
  | HiValueString Text
  | HiValueBytes ByteString
  | HiValueList (Seq HiValue)
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass Serialise

data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Show, Generic)

data HiError
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show, Eq)

data HiAction
  = HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass Serialise

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

showFun :: HiFun -> String
showFun HiFunAdd            = "add"
showFun HiFunAnd            = "and"
showFun HiFunDiv            = "div"
showFun HiFunEquals         = "equals"
showFun HiFunGreaterThan    = "greater-than"
showFun HiFunIf             = "if"
showFun HiFunLessThan       = "less-than"
showFun HiFunMul            = "mul"
showFun HiFunNot            = "not"
showFun HiFunNotEquals      = "not-equals"
showFun HiFunNotGreaterThan = "not-greater-than"
showFun HiFunNotLessThan    = "not-less-than"
showFun HiFunOr             = "or"
showFun HiFunSub            = "sub"
showFun HiFunLength         = "length"
showFun HiFunToUpper        = "to-upper"
showFun HiFunToLower        = "to-lower"
showFun HiFunReverse        = "reverse"
showFun HiFunTrim           = "trim"
showFun HiFunList           = "list"
showFun HiFunRange          = "range"
showFun HiFunFold           = "fold"
showFun HiFunPackBytes      = "pack-bytes"
showFun HiFunUnpackBytes    = "unpack-bytes"
showFun HiFunEncodeUtf8     = "encode-utf8"
showFun HiFunDecodeUtf8     = "decode-utf8"
showFun HiFunZip            = "zip"
showFun HiFunUnzip          = "unzip"
showFun HiFunSerialise      = "serialise"
showFun HiFunDeserialise    = "deserialise"
showFun HiFunRead           = "read"
showFun HiFunWrite          = "write"
showFun HiFunMkDir          = "mkdir"
showFun HiFunChDir          = "cd"
showFun HiFunParseTime      = "parse-time"
showFun HiFunRand           = "rand"
showFun HiFunEcho           = "echo"
showFun HiFunCount          = "count"
showFun HiFunKeys           = "keys"
showFun HiFunValues         = "values"
showFun HiFunInvert         = "invert"

isBinary :: HiFun -> Bool
isBinary = \case
  HiFunDiv            -> True  ------ Made
  HiFunMul            -> True  ------ Made
  HiFunAdd            -> True  ------ Made
  HiFunSub            -> True  ------ Made
  HiFunNot            -> False ------ Done
  HiFunAnd            -> True  ------ Already visited
  HiFunOr             -> True  ------ Already visited
  HiFunLessThan       -> True  ------ Made
  HiFunGreaterThan    -> True  ------ Made
  HiFunEquals         -> True  ------ Made
  HiFunNotLessThan    -> True  ------ Made
  HiFunNotGreaterThan -> True  ------ Made
  HiFunNotEquals      -> True  ------ Made
  HiFunIf             -> False ------ Already visited
  HiFunLength         -> False ------ Done
  HiFunToUpper        -> False ------ Done
  HiFunToLower        -> False ------ Done
  HiFunReverse        -> False ------ Done
  HiFunTrim           -> False ------ Done
  HiFunList           -> False ------ Already visited
  HiFunRange          -> True  ------ Made
  HiFunFold           -> True  
  HiFunPackBytes      -> False ------ Done
  HiFunUnpackBytes    -> False ------ Done
  HiFunEncodeUtf8     -> False ------ Done
  HiFunDecodeUtf8     -> False ------ Done
  HiFunZip            -> False ------ Done
  HiFunUnzip          -> False ------ Done
  HiFunSerialise      -> False ------ Done
  HiFunDeserialise    -> False ------ Done
  HiFunRead           -> False ------ Done
  HiFunWrite          -> True  ------ Made
  HiFunMkDir          -> False ------ Done
  HiFunChDir          -> False ------ Done
  HiFunParseTime      -> False ------ Done
  HiFunRand           -> True  ------ Made
  HiFunEcho           -> False ------ Done
  HiFunCount          -> False ------ Done
  HiFunKeys           -> False ------ Done
  HiFunValues         -> False ------ Done
  HiFunInvert         -> False ------ Done
