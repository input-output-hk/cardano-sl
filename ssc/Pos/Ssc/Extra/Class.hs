{-# LANGUAGE TypeFamilies #-}

-- | Class for in-memory state of SSC. Doesn't depend on concrete SSC.

module Pos.Ssc.Extra.Class
       ( MonadSscMem
       , askSscMem
       , SscMemTag
       ) where

import           EtherCompat

import           Pos.Ssc.Extra.Types (SscState)

data SscMemTag

type MonadSscMem ssc ctx m = MonadCtx ctx SscMemTag (SscState ssc) m

askSscMem :: MonadSscMem ssc ctx m => m (SscState ssc)
askSscMem = askCtx @SscMemTag
