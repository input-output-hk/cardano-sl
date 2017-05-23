module Main
  ( main
  ) where

import qualified Data.ByteString.Char8       as BS
import           Network.Kademlia.HashNodeId (HashId (..), Nonce (..), genNonce,
                                              hashAddress)
import           Serokell.Util.Base64        (encodeUrl)
import           Universum

import           Pos.Crypto                  (runSecureRandom)

nonceLen :: Int
nonceLen = 14

generateKey :: BS.ByteString -> IO (Maybe BS.ByteString)
generateKey nonce =
  if length nonce /= nonceLen
  then if nonce == "-"
          then Just <$> randomDHTKey
          else pure Nothing
  else pure $ Just . unHashId . hashAddress $ Nonce nonce

unHashId :: HashId -> BS.ByteString
unHashId (HashId bs) = bs

randomDHTKey :: IO BS.ByteString
randomDHTKey = unHashId . hashAddress <$> runSecureRandom genNonce

processArg :: [Char] -> IO ()
processArg arg = do
    key <- generateKey $ BS.pack arg
    putStrLn . fromMaybe "Invalid nonce length" . fmap encodeUrl $ key

main :: IO ()
main = mapM_ processArg =<< getArgs
