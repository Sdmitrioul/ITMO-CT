{-# LANGUAGE LambdaCase #-}

module HW3.Helpers where

import HW3.Base
import Data.Sequence (Seq, fromList)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString (ByteString)
import Data.Time (UTCTime)
import Data.Text.Encoding (decodeUtf8')
import Data.Ratio (denominator, numerator)
import Data.Maybe (fromMaybe)
import Data.Foldable (toList)
import qualified Data.ByteString as B (pack, unpack)
import qualified Data.Text as T (Text, unpack)
import qualified Data.Map as M (Map, fromListWith, map, toList, empty, insertWith)
import qualified Data.ListLike as LL
import Codec.Compression.Zlib
import Text.Read (readMaybe)
import Control.Monad.Trans.Except

invertHiDict :: HiMonad m => M.Map HiValue HiValue -> ExceptT HiError m HiValue
invertHiDict d = return $ HiValueDict $ M.map (HiValueList . fromList) $ foldl (\dict (k, v) -> M.insertWith (++) v [k] dict) M.empty $ M.toList d

countHiElements :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
countHiElements = return . HiValueDict . M.map HiValueNumber . M.map fromInteger . M.fromListWith (+) . flip zip (repeat 1)

parseHiTime :: HiMonad m => T.Text -> ExceptT HiError m HiValue
parseHiTime s = case readMaybe $ T.unpack s :: Maybe UTCTime of
    Nothing   -> return HiValueNull
    Just time -> return $ HiValueTime time

createHiAction :: HiMonad m => (FilePath -> HiAction) -> T.Text -> ExceptT HiError m HiValue
createHiAction f s = return $ HiValueAction $ f $ T.unpack s

unzipHiBytes :: HiMonad m => ByteString -> ExceptT HiError m HiValue
unzipHiBytes b = return $ HiValueBytes $ toStrict $ decompressWith defaultDecompressParams $ fromStrict b

zipHiBytes :: HiMonad m => ByteString -> ExceptT HiError m HiValue
zipHiBytes b =  return $ HiValueBytes $ toStrict $ compressWith defaultCompressParams {compressLevel = bestCompression} $ fromStrict b

decodeHiBytes :: HiMonad m => ByteString -> ExceptT HiError m HiValue
decodeHiBytes b = case decodeUtf8' b of
  Right res -> return $ HiValueString res
  Left _    -> return HiValueNull

packHiBytes :: HiMonad m => Seq HiValue -> ExceptT HiError m HiValue
packHiBytes l = do
  values <- mapM checkAndGetIntNum l
  byt <- mapM checkIsByte values
  return $ HiValueBytes $ B.pack $ map fromIntegral $ toList byt

unpackHiBytes :: HiMonad m => ByteString -> ExceptT HiError m HiValue
unpackHiBytes b = return $ HiValueList $ fromList $ map (HiValueNumber . fromIntegral) $ B.unpack b


checkIsByte :: HiMonad m => Integer -> ExceptT HiError m Integer
checkIsByte num = if num <= 255 || num >= 0 then return num else throwE HiErrorInvalidArgument

reverseOfHi :: (LL.ListLike l v, HiMonad m) => (l -> HiValue) -> l -> ExceptT HiError m HiValue
reverseOfHi f = return . f . LL.reverse

lengthOfHi :: (LL.ListLike l v, HiMonad m) => l -> ExceptT HiError m HiValue
lengthOfHi = return . HiValueNumber . fromIntegral . LL.length

findByIndex :: (LL.ListLike l v, HiMonad m) => l -> HiValue -> (v -> HiValue) -> ExceptT HiError m HiValue
findByIndex list ind f = do
  pos <- checkAndGetIntNum ind
  if pos >= 0 && fromInteger pos < LL.length list
    then return . f $ LL.index list $ fromInteger pos
    else return HiValueNull

sliceElements :: (LL.ListLike l v, HiMonad m) => l -> [HiValue] -> (l -> HiValue) -> ExceptT HiError m HiValue
sliceElements list [left, right] f = do
  let lLength = toInteger $ LL.length list
  leftCalc <- fromMaybe 0 <$> checkAndGetIntOrNull left
  rightCalc <- fromMaybe lLength <$> checkAndGetIntOrNull right
  let begin = getPos leftCalc lLength
  let end = getPos rightCalc lLength
  if begin < end
    then return $ f $ LL.take (fromInteger $ end - begin) $ LL.drop (fromInteger begin) list
    else return $ f $ LL.empty
  where
    getPos :: Integer -> Integer -> Integer
    getPos pos mLen = if pos < 0 then pos + mLen else pos  
sliceElements _ _ _                = throwE HiErrorArityMismatch

checkAndGetNum :: HiMonad m => HiValue -> ExceptT HiError m Rational
checkAndGetNum (HiValueNumber num) = return num
checkAndGetNum _                   = throwE HiErrorInvalidArgument 

checkAndGetInt :: HiMonad m => Rational -> ExceptT HiError m Integer
checkAndGetInt num = case (numerator num, denominator num) of
    (n, 1) -> return n
    _      -> throwE HiErrorInvalidArgument 

checkAndGetIntNum :: HiMonad m => HiValue -> ExceptT HiError m Integer
checkAndGetIntNum (HiValueNumber num) = checkAndGetInt num
checkAndGetIntNum _                   = throwE HiErrorInvalidArgument 

checkAndGetIntOrNull :: HiMonad m => HiValue -> ExceptT HiError m (Maybe Integer)
checkAndGetIntOrNull (HiValueNumber num) = Just <$> checkAndGetInt num
checkAndGetIntOrNull HiValueNull         = return Nothing
checkAndGetIntOrNull _                   = throwE HiErrorInvalidArgument
