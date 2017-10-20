{-# LANGUAGE TypeFamilies #-}

-- | Class for in-memory state of SSC. Doesn't depend on concrete SSC.

module Pos.Ssc.Extra.Class
       ( MonadSscMem
       , askSscMem
       , SscMemTag
       ) where

import           Ether.Internal      (HasLens (..))
import           Universum

import           Pos.Ssc.Types       (SscState)

data SscMemTag

type MonadSscMem ctx m = (MonadReader ctx m, HasLens SscMemTag ctx SscState)

askSscMem :: MonadSscMem ctx m => m SscState
askSscMem = view (lensOf @SscMemTag)
