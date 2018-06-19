-- | Dummy values used in tests (replacing `configuration.yaml`)

module Test.Pos.Crypto.Dummy
       ( dummyProtocolMagic
       ) where

import           Pos.Crypto (ProtocolMagic (..))

dummyProtocolMagic :: ProtocolMagic
dummyProtocolMagic = ProtocolMagic 55550001
