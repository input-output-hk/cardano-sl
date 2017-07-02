{-# LANGUAGE TypeFamilies #-}

-- | Class for in-memory state of SSC. Doesn't depend on concrete SSC.

module Pos.Ssc.Extra.Class
       ( MonadSscMem
       , askSscMem
       , SscMemTag
       ) where

import           Ether.Internal      (HasLens (..))
import           Universum

import           Pos.Ssc.Extra.Types (SscState)

data SscMemTag

type MonadSscMem ssc ctx m = (MonadReader ctx m, HasLens SscMemTag ctx (SscState ssc))

askSscMem :: MonadSscMem ssc ctx m => m (SscState ssc)
askSscMem = view (lensOf @SscMemTag)
