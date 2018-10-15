-- | Dummy values used in tests (replacing `configuration.yaml`)

module Test.Pos.Crypto.Dummy
       ( dummyProtocolMagic
       , dummyProtocolMagicId
       , dummyRequiresNetworkMagic
       ) where

import           Pos.Crypto (ProtocolMagic (..), ProtocolMagicId (..),
                     RequiresNetworkMagic (..))

dummyProtocolMagic :: ProtocolMagic
dummyProtocolMagic = ProtocolMagic dummyProtocolMagicId RequiresNoMagic

dummyProtocolMagicId :: ProtocolMagicId
dummyProtocolMagicId = ProtocolMagicId 55550001

dummyRequiresNetworkMagic :: RequiresNetworkMagic
dummyRequiresNetworkMagic = RequiresNoMagic
