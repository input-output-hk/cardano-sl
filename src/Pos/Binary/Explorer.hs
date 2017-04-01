-- | Binary instances for explorer types

module Pos.Binary.Explorer () where

import           Data.Binary.Get         (label)
import           Universum

import           Pos.Binary.Class        (Bi (..))
import           Pos.Binary.Txp          ()
import           Pos.Explorer.Core.Types (TxExtra (..))

instance Bi TxExtra where
    get = label "TxExtra" $
          TxExtra <$> get <*> get <*> get
    put TxExtra {..} = put teBlockchainPlace >>
                       put teReceivedTime >>
                       put teInputOutputs
