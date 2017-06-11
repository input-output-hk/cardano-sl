-- | Binary instances for explorer types

module Pos.Binary.Explorer () where

import           Universum

import           Pos.Binary.Class        (Bi (..), label, putField)
import           Pos.Binary.Txp          ()
import           Pos.Explorer.Core.Types (TxExtra (..))

instance Bi TxExtra where
    get = label "TxExtra" $
          TxExtra <$> get <*> get <*> get
    sizeNPut = putField teBlockchainPlace >>
               putField teReceivedTime >>
               putField teInputOutputs
