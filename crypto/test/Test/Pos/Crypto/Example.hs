module Test.Pos.Crypto.Example
    ( exampleProtocolMagic0
    , exampleProtocolMagic1
    , exampleProtocolMagic2
    ) where

import           Pos.Crypto (ProtocolMagic (..))

--------------------------------------------------------------------------------
-- Example golden datatypes
--------------------------------------------------------------------------------

exampleProtocolMagic0 :: ProtocolMagic
exampleProtocolMagic0 = ProtocolMagic 31337

exampleProtocolMagic1 :: ProtocolMagic
exampleProtocolMagic1 = ProtocolMagic 2147000001

exampleProtocolMagic2 :: ProtocolMagic
exampleProtocolMagic2 = ProtocolMagic (- 58952)
