{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Char8  as BS
import           Network.Kademlia.HashNodeId (Nonce (..), HashId (..), hashAddress)
import           Universum

nonceLen :: Int
nonceLen = 14

generateKey :: [Char] -> Maybe [Char]
generateKey nonce =
  if length nonce /= nonceLen
  then Nothing
  else let (HashId bs) = hashAddress . Nonce $ BS.pack nonce
       in Just $ BS.unpack bs

main :: IO ()
main = mapM_ ((putStrLn :: [Char] -> IO ()) . show . generateKey) =<< getArgs
