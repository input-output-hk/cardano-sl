module Util.Version where

import Data.Function.Uncurried (Fn0, runFn0)

foreign import versionImpl :: Fn0 Int

version :: Int
version = runFn0 versionImpl

foreign import commitHashImpl :: Fn0 String

commitHash :: String
commitHash = runFn0 commitHashImpl
