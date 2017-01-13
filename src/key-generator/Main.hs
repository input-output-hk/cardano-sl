{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Char8  as BS
import           Network.Kademlia.HashNodeId (Nonce (..), HashId (..), hashAddress)
import Serokell.Util.Base64 (encodeUrl)

import           Universum

nonceLen :: Int
nonceLen = 14

generateKey :: BS.ByteString -> Maybe BS.ByteString
generateKey nonce =
  if length nonce /= nonceLen
  then Nothing
  else let (HashId bs) = hashAddress $ Nonce nonce
       in Just bs

main :: IO ()
main = mapM_ (putStrLn
              . fromMaybe "Invalid nonce length"
              . fmap encodeUrl
              . generateKey
              . BS.pack) =<< getArgs
