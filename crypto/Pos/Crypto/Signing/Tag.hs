module Pos.Crypto.Signing.Tag
       ( signTag
       , module Pos.Crypto.Signing.Types.Tag
       ) where

import           Universum

import qualified Pos.Binary.Class as Bi
import           Pos.Crypto.Configuration (ProtocolMagic (..))
import           Pos.Crypto.Signing.Types.Tag

-- | Get magic bytes corresponding to a 'SignTag'. Guaranteed to be different
-- (and begin with a different byte) for different tags.
signTag :: ProtocolMagic -> SignTag -> ByteString
signTag protocolMagic = \case
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
    network = Bi.serialize' (getProtocolMagic protocolMagic)
