{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Cardano.Crypto.Wallet (unXPrv)
import qualified Data.ByteString as BS
import qualified Data.Text.Lazy.Builder as T
import           Formatting (Format, bprint, fprint, hex, later, shown, (%))
import           Formatting.Internal.Raw (left)
import           Pos.Crypto (EncryptedSecretKey (eskHash, eskPayload),
                     getEncryptedPass)
import           Pos.Util.Trace (fromTypeclassWlog)
import           Pos.Util.UserSecret
import           Pos.Util.Wlog.Compatibility (setupTestLogging)
import           Universum

hexString :: Format r (ByteString -> r)
hexString = later f
  where
    f :: ByteString -> T.Builder
    f bs = foldl' f2 "" $ BS.unpack bs
    f2 :: T.Builder -> Word8 -> T.Builder
    f2 str byte = str <> (left 2 '0' (bprint hex byte))

extract :: EncryptedSecretKey -> FilePath -> IO ()
extract esk path = do
  let
    (epriv,remain) = BS.splitAt 64 $ unXPrv $ eskPayload esk
    (pub,chaincode) = BS.splitAt 32 remain
  fprint ("XPrv: " % hexString % ", pub: " % hexString % ", CC: " % hexString % "\n") epriv pub chaincode
  fprint ("pass hash: " % shown % "\n") (getEncryptedPass $ eskHash esk)
  let
    newkey = (defaultUserSecret & usWallet .~ Just (WalletUserSecret esk "exported key" [] [])) & usPath .~ path
  writeUserSecret newkey

main :: IO ()
main = do
  setupTestLogging
  [ path ] <- getArgs
  originalKey <- readUserSecret fromTypeclassWlog path
  let
    zero :: Int
    zero = 0
  forM_ (zip (view usKeys originalKey) [zero..]) $ \(key, x) -> do
    fprint ("extracting key#" % shown % "\n") x
    extract key ("output-" <> show x <> ".key")
  case (view usWallet originalKey) of
    Nothing -> fprint "no wallet key"
    Just key -> do
      fprint "extracting wallet key"
      extract (view wusRootKey key) "output-wallet.key"
