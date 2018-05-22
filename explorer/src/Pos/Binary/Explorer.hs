{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Binary instances for explorer types

module Pos.Binary.Explorer () where

import           Universum

import           Pos.Binary ()
import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Core (HeaderHash, Timestamp, TxUndo)
import           Pos.Explorer.Core.Types (TxExtra (..))

deriveSimpleBi ''TxExtra [
    Cons 'TxExtra [
        Field [| teBlockchainPlace :: Maybe (HeaderHash, Word32) |],
        Field [| teReceivedTime    :: Maybe Timestamp            |],
        Field [| teInputOutputs    :: TxUndo                     |]
    ]]
