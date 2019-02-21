{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import qualified Data.Text as T
import           Pos.Util.Trace (fromTypeclassWlog)
import           Pos.Util.UserSecret
import           Universum

main :: IO ()
main = do
  [ path ] <- getArgs
  originalKey <- readUserSecret fromTypeclassWlog path
  let
    zero :: Int
    zero = 0
  forM_ (zip (view usKeys originalKey) [zero..]) $ \(key, x) -> do
    let
      newkey = (defaultUserSecret & usWallet .~ Just (WalletUserSecret key (T.pack "head key") [] [])) & usPath .~ ("output-" <> show x <> ".key")
    writeUserSecret newkey
