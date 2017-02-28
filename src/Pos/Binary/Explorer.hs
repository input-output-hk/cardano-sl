-- | Binary instances for explorer types

module Pos.Binary.Explorer () where

import           Data.Binary.Get    (label)
import           Universum

import           Pos.Binary.Class   (Bi (..))
import           Pos.Binary.Txp     ()
import           Pos.Types.Explorer (TxExtra (..))

instance Bi TxExtra where
    get = label "TxExtra" $
          TxExtra <$> get <*> get
    put TxExtra {..} = put teBlockchainPlace >> put teInputOutputs
