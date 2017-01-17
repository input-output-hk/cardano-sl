module Util.Version where

foreign import versionImpl :: Int

version :: Int
version = versionImpl

foreign import commitHashImpl :: String

commitHash :: String
commitHash = commitHashImpl
