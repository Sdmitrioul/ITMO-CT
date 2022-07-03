{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as Set
import qualified Data.Text as T
import HW3.Action (HIO (runHIO), HiPermission (..))
import HW3.Base (HiError, HiValue)
import System.IO.Error (catchIOError)
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Pretty (prettyValue)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing    -> return ()
        Just ":q"  -> return ()
        Just ""    -> loop
        Just input -> do
          case parse input of
            Left exc   -> outputStrLn $ errorBundlePretty exc
            Right expr -> do
              res <- liftIO (try (runHIO (eval expr) (Set.fromList [AllowRead, AllowWrite, AllowTime]))
                :: IO (Either SomeException (Either HiError HiValue)))
              case res of
                Left err          -> outputStrLn ("I/O Exception: " ++ show err)
                Right (Left err)  -> outputStrLn ("Exception: " ++ show err)
                Right (Right val) -> outputStrLn (show $ prettyValue val)
          loop
