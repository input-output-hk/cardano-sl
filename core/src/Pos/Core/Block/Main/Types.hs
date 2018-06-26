-- | Types defining the main blockchain.

module Pos.Core.Block.Main.Types
       ( MainProof (..)
       , MainExtraHeaderData (..)
       , MainBody (..)
       , MainExtraBodyData (..)

       , BlockHeaderAttributes
       , BlockBodyAttributes
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable
import           Fmt (genericF)
import           Formatting (bprint, build, builder, (%))

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..),
                                   deriveSimpleBi, encodeListLen,
                                   enforceSize)
import           Pos.Binary.Core.Ssc ()
import           Pos.Binary.Core.Update ()
import           Pos.Core.Delegation (DlgPayload)
import           Pos.Core.Ssc (SscPayload, SscProof)
import           Pos.Core.Txp (TxPayload, TxProof)
import           Pos.Core.Update (BlockVersion, SoftwareVersion, UpdatePayload, UpdateProof)
import           Pos.Crypto (Hash)
import           Pos.Data.Attributes (Attributes, areAttributesKnown)

-- | Proof of everything contained in the payload.
data MainProof = MainProof
    { mpTxProof       :: !TxProof
    , mpMpcProof      :: !SscProof
    , mpProxySKsProof :: !(Hash DlgPayload)
    , mpUpdateProof   :: !UpdateProof
    } deriving (Eq, Show, Generic)

instance NFData MainProof

instance Buildable MainProof where
    build = genericF

instance Bi MainProof where
    encode bc =  encodeListLen 4
              <> encode (mpTxProof bc)
              <> encode (mpMpcProof bc)
              <> encode (mpProxySKsProof bc)
              <> encode (mpUpdateProof bc)
    decode = do
        enforceSize "Core.BodyProof MainBlockChain" 4
        MainProof <$> decode <*>
                         decode <*>
                         decode <*>
                         decode


-- | Represents main block header attributes: map from 1-byte integer to
-- arbitrary-type value. To be used for extending header with new
-- fields via softfork.
type BlockHeaderAttributes = Attributes ()

-- | Represents main block header extra data
data MainExtraHeaderData = MainExtraHeaderData
    { -- | Version of block.
      _mehBlockVersion    :: !BlockVersion
    , -- | Software version.
      _mehSoftwareVersion :: !SoftwareVersion
    , -- | Header attributes
      _mehAttributes      :: !BlockHeaderAttributes
    , -- | Extra body data Hash
      _mehEBDataProof     :: !(Hash MainExtraBodyData)
    } deriving (Eq, Show, Generic)

instance NFData MainExtraHeaderData

instance Buildable MainExtraHeaderData where
    build MainExtraHeaderData {..} =
      bprint ( "    block: v"%build%"\n"
             % "    software: "%build%"\n"
             % builder
             )
            _mehBlockVersion
            _mehSoftwareVersion
            formattedExtra
      where
        formattedExtra
            | areAttributesKnown _mehAttributes = mempty
            | otherwise = bprint ("    attributes: "%build%"\n") _mehAttributes

-- | In our cryptocurrency, body consists of payloads of all block
-- components.
data MainBody = MainBody
    { -- | Txp payload.
      _mbTxPayload     :: !TxPayload
    , -- | Ssc payload.
      _mbSscPayload    :: !SscPayload
    , -- | Heavyweight delegation payload (no-ttl certificates).
      _mbDlgPayload    :: !DlgPayload
      -- | Additional update information for the update system.
    , _mbUpdatePayload :: !UpdatePayload
    } deriving (Eq, Show, Generic, Typeable)

instance Bi MainBody where
    encode bc =  encodeListLen 4
              <> encode (_mbTxPayload  bc)
              <> encode (_mbSscPayload bc)
              <> encode (_mbDlgPayload bc)
              <> encode (_mbUpdatePayload bc)
    decode = do
        enforceSize "Body MainBlockchain" 4
        MainBody <$> decode <*>
                        decode <*>
                        decode <*>
                        decode

instance NFData MainBody

-- | Represents main block body attributes: map from 1-byte integer to
-- arbitrary-type value. To be used for extending block with new
-- fields via softfork.
type BlockBodyAttributes = Attributes ()

-- | Represents main block extra data
newtype MainExtraBodyData = MainExtraBodyData
    { _mebAttributes  :: BlockBodyAttributes
    } deriving (Eq, Show, Generic, NFData)

instance Buildable MainExtraBodyData where
    build (MainExtraBodyData attrs)
        | areAttributesKnown attrs = "no extra data"
        | otherwise = bprint ("extra data has attributes: "%build) attrs

deriveSimpleBi ''MainExtraHeaderData [
    Cons 'MainExtraHeaderData [
        Field [| _mehBlockVersion    :: BlockVersion              |],
        Field [| _mehSoftwareVersion :: SoftwareVersion           |],
        Field [| _mehAttributes      :: BlockHeaderAttributes  |],
        Field [| _mehEBDataProof     :: Hash MainExtraBodyData |]
    ]]

deriveSimpleBi ''MainExtraBodyData [
    Cons 'MainExtraBodyData [
        Field [| _mebAttributes :: BlockBodyAttributes |]
    ]]
