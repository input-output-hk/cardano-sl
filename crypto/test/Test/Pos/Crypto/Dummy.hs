-- | Dummy values used in tests (replacing `configuration.yaml`)

module Test.Pos.Crypto.Dummy
       ( dummyProtocolMagic
       , dummyProtocolMagicId
       ) where

import           Pos.Crypto (ProtocolMagic (..), ProtocolMagicId (..), RequiresNetworkMagic (..))

dummyProtocolMagic :: ProtocolMagic
dummyProtocolMagic = ProtocolMagic dummyProtocolMagicId NMMustBeNothing

dummyProtocolMagicId :: ProtocolMagicId
dummyProtocolMagicId = ProtocolMagicId 55550001
