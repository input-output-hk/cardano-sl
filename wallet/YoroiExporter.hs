{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main (main) where

import           Cardano.Crypto.Wallet (unXPrv, xPrvChangePass)
import           Crypto.Hash (hash)
import           Crypto.Hash.Algorithms (Blake2b_256)
import           Data.Aeson
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Builder as T
import           Formatting (Format, bprint, hex, later, sformat, (%))
import           Formatting.Internal.Raw (left)
import           Pos.Crypto (EncryptedSecretKey (eskPayload), checkPassMatches)
import           Pos.Util.Trace (fromTypeclassWlog)
import           Pos.Util.UserSecret
import           Pos.Util.Wlog.Compatibility (setupTestLogging)
import           Safe
import           Universum

hexString :: Format r (ByteString -> r)
hexString = later f
  where
    f :: ByteString -> T.Builder
    f bs = foldl' f2 "" $ BS.unpack bs
    f2 :: T.Builder -> Word8 -> T.Builder
    f2 str byte = str <> (left 2 '0' (bprint hex byte))

data Res = InvalidArgs | InvalidIndex | InvalidSpendingPw | MasterKey Text deriving (Show, Generic)

instance ToJSON Res where
  toJSON = genericToJSON defaultOptions

go :: [ String ] -> IO Res
go [ path, index, pw ] = do
  originalKey <- readUserSecret fromTypeclassWlog path
  let
    maybeIndex :: Maybe Int
    maybeIndex = readMay index
  let
    pwhash = ByteArray.convert (hash @ByteString @Blake2b_256 $ T.encodeUtf8 $ T.pack pw)
  case maybeIndex of
    Nothing -> pure InvalidIndex
    Just index' -> do
      let maybeEsk = atMay (view usKeys originalKey) index'
      case maybeEsk of
        Just esk -> do
          let
            matches :: Bool
            matches = maybe False (const True) (checkPassMatches pwhash esk)
          case matches of
            True -> do
              let
                decryptedXprv = xPrvChangePass pwhash (mempty :: ByteString) $ eskPayload esk
                (epriv,remain) = BS.splitAt 64 $ unXPrv $ decryptedXprv
                (_pub,chaincode) = BS.splitAt 32 remain
              pure $ MasterKey $ sformat (hexString % hexString) epriv chaincode
            False -> pure InvalidSpendingPw
        Nothing -> pure InvalidIndex
go _ = pure InvalidArgs

main :: IO ()
main = do
  setupTestLogging
  args <- getArgs
  result <- go args
  putStrLn $ encode result
