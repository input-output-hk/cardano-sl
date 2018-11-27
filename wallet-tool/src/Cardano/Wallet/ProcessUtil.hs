module Cardano.Wallet.ProcessUtil
  ( checkProcessExists
  , ProcessID
  ) where

import Universum
import System.Posix.Types
import System.Posix.Process
import Control.Exception (IOException)

checkProcessExists :: Maybe ProcessID -> IO Bool
checkProcessExists Nothing = pure True
checkProcessExists (Just pid) = hasGroup `catch` (\(_ :: IOException) -> pure False)
  where
    hasGroup = (> 0) <$> getProcessGroupIDOf pid
