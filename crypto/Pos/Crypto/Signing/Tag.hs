module Pos.Crypto.Signing.Tag
       ( signTag
       , module Pos.Crypto.Signing.Types.Tag
       ) where

import           Universum

import           Pos.Binary.Class (serialize')
import           Pos.Crypto.Configuration (HasCryptoConfiguration, ProtocolMagic (..),
                                           protocolMagic)
import           Pos.Crypto.Signing.Types.Tag

-- | Get magic bytes corresponding to a 'SignTag'. Guaranteed to be different
-- (and begin with a different byte) for different tags.
signTag
    :: HasCryptoConfiguration
    => SignTag -> ByteString
signTag = \case
    SignForTestingOnly -> "\x00"
    SignTx             -> "\x01" <> network
    SignRedeemTx       -> "\x02" <> network
    SignVssCert        -> "\x03" <> network
    SignUSProposal     -> "\x04" <> network
    SignCommitment     -> "\x05" <> network
    SignUSVote         -> "\x06" <> network
    SignMainBlock      -> "\x07" <> network
    SignMainBlockLight -> "\x08" <> network
    SignMainBlockHeavy -> "\x09" <> network
    SignProxySK        -> "\x0a" <> network
  where
    -- TODO: we could just serialize' protocolMagic directly here, but then
    -- we'd have to import Pos.Binary.Crypto and it would lead to an import
    -- cycle. When the module hierarchy is redesigned (or checks are moved
    -- from Bi instances to some other place), this wouldn't be needed.
    network = serialize' (getProtocolMagic protocolMagic)
