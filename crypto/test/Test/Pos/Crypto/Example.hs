module Test.Pos.Crypto.Example
    ( exampleProtocolMagic0
    , exampleProtocolMagic1
    , exampleProtocolMagic2
    , exampleProtocolMagic3
    , exampleProtocolMagic4
    ) where

import           Pos.Crypto (ProtocolMagic (..), ProtocolMagicId (..),
                     RequiresNetworkMagic (..))

--------------------------------------------------------------------------------
-- Example golden datatypes
--------------------------------------------------------------------------------

exampleProtocolMagic0 :: ProtocolMagic
exampleProtocolMagic0 = ProtocolMagic (ProtocolMagicId 31337) RequiresMagic

exampleProtocolMagic1 :: ProtocolMagic
exampleProtocolMagic1 = ProtocolMagic (ProtocolMagicId 2147000001) RequiresMagic

exampleProtocolMagic2 :: ProtocolMagic
exampleProtocolMagic2 = ProtocolMagic (ProtocolMagicId (- 58952)) RequiresMagic

exampleProtocolMagic3 :: ProtocolMagic
exampleProtocolMagic3 = ProtocolMagic (ProtocolMagicId (31337)) RequiresMagic

exampleProtocolMagic4 :: ProtocolMagic
exampleProtocolMagic4 = ProtocolMagic (ProtocolMagicId (- 500)) RequiresNoMagic

