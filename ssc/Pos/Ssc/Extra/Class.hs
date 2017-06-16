{-# LANGUAGE TypeFamilies #-}

-- | Class for in-memory state of SSC. Doesn't depend on concrete SSC.

module Pos.Ssc.Extra.Class
       ( MonadSscMem
       , askSscMem
       , SscMemTag
       ) where

import qualified Ether

import           Pos.Ssc.Extra.Types (SscState)

data SscMemTag

type MonadSscMem ssc = Ether.MonadReader SscMemTag (SscState ssc)

askSscMem :: MonadSscMem ssc m => m (SscState ssc)
askSscMem = Ether.ask @SscMemTag
