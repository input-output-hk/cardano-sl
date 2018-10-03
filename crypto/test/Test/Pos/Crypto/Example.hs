module Test.Pos.Crypto.Example
    ( exampleProtocolMagic0
    , exampleProtocolMagic1
    , exampleProtocolMagic2
    ) where

import           Pos.Crypto (ProtocolMagic (..), ProtocolMagicId (..),
                     RequiresNetworkMagic (..))

--------------------------------------------------------------------------------
-- Example golden datatypes
--------------------------------------------------------------------------------

exampleProtocolMagic0 :: ProtocolMagic
exampleProtocolMagic0 = ProtocolMagic (ProtocolMagicId 31337) NMMustBeJust

exampleProtocolMagic1 :: ProtocolMagic
exampleProtocolMagic1 = ProtocolMagic (ProtocolMagicId 2147000001) NMMustBeJust

exampleProtocolMagic2 :: ProtocolMagic
exampleProtocolMagic2 = ProtocolMagic (ProtocolMagicId (- 58952)) NMMustBeJust
