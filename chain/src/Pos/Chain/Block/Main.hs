{-# LANGUAGE RecordWildCards #-}

-- | Types defining the main blockchain.

module Pos.Chain.Block.Main
       ( MainProof (..)
       , mkMainProof
       , checkMainProof

       , MainExtraHeaderData (..)
       , mehBlockVersion
       , mehSoftwareVersion
       , mehAttributes
       , mehEBDataProof
       , verifyMainExtraHeaderData

       , MainBody (..)
       , mbSscPayload
       , mbTxPayload
       , mbDlgPayload
       , mbUpdatePayload
       , mbTxs
       , mbWitnesses
       , verifyMainBody

       , MainExtraBodyData (..)
       , mebAttributes

       , BlockHeaderAttributes
       , BlockBodyAttributes
       ) where

import           Universum

import           Control.Lens (makeLenses)
import           Control.Monad.Except (MonadError)
import           Data.SafeCopy (SafeCopy (..), base, contain,
                     deriveSafeCopySimple, safeGet, safePut)
import           Fmt (genericF)
import           Formatting (bprint, build, builder, (%))
import qualified Formatting.Buildable as Buildable

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..),
                     deriveSimpleBi, encodeListLen, enforceSize)
import           Pos.Chain.Block.Util (checkBodyProof)
import           Pos.Chain.Delegation.Payload (DlgPayload, checkDlgPayload)
import           Pos.Chain.Ssc.Payload (SscPayload, checkSscPayload)
import           Pos.Chain.Ssc.Proof (SscProof, mkSscProof)
import           Pos.Chain.Txp.Tx (Tx, TxValidationRules)
import           Pos.Chain.Txp.TxPayload (TxPayload, checkTxPayload, txpTxs,
                     txpWitnesses)
import           Pos.Chain.Txp.TxProof (TxProof, mkTxProof)
import           Pos.Chain.Txp.TxWitness (TxWitness)
import           Pos.Chain.Update.BlockVersion (BlockVersion,
                     HasBlockVersion (..))
import           Pos.Chain.Update.Payload (UpdatePayload, checkUpdatePayload)
import           Pos.Chain.Update.Proof (UpdateProof, mkUpdateProof)
import           Pos.Chain.Update.SoftwareVersion (HasSoftwareVersion (..),
                     SoftwareVersion, checkSoftwareVersion)
import           Pos.Core.Attributes (Attributes, areAttributesKnown)
import           Pos.Crypto (Hash, ProtocolMagic, hash)

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

instance SafeCopy MainProof where
    getCopy = contain $ do
        mpTxProof <- safeGet
        mpMpcProof      <- safeGet
        mpProxySKsProof <- safeGet
        mpUpdateProof   <- safeGet
        return $! MainProof{..}
    putCopy MainProof {..} = contain $ do
        safePut mpTxProof
        safePut mpMpcProof
        safePut mpProxySKsProof
        safePut mpUpdateProof

mkMainProof :: MainBody -> MainProof
mkMainProof MainBody {..} = MainProof
    { mpTxProof       = mkTxProof _mbTxPayload
    , mpMpcProof      = mkSscProof _mbSscPayload
    , mpProxySKsProof = hash _mbDlgPayload
    , mpUpdateProof   = mkUpdateProof _mbUpdatePayload
    }

checkMainProof :: MonadError Text m => MainBody -> MainProof -> m ()
checkMainProof = checkBodyProof mkMainProof

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

verifyMainExtraHeaderData
    :: ( MonadError Text m )
    => MainExtraHeaderData
    -> m ()
verifyMainExtraHeaderData MainExtraHeaderData {..} = do
    checkSoftwareVersion _mehSoftwareVersion

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

instance SafeCopy MainBody where
    getCopy = contain $ do
        _mbTxPayload     <- safeGet
        _mbSscPayload    <- safeGet
        _mbDlgPayload    <- safeGet
        _mbUpdatePayload <- safeGet
        return $! MainBody{..}
    putCopy MainBody {..} = contain $ do
        safePut _mbTxPayload
        safePut _mbSscPayload
        safePut _mbDlgPayload
        safePut _mbUpdatePayload

-- | Verify the body of a block. There are no internal consistency checks,
-- it's just a verification of its sub-components (payloads).
verifyMainBody
    :: MonadError Text m
    => ProtocolMagic
    -> TxValidationRules
    -> MainBody
    -> m ()
verifyMainBody pm txValRules MainBody {..} = do
    checkTxPayload txValRules _mbTxPayload
    checkSscPayload pm _mbSscPayload
    checkDlgPayload pm _mbDlgPayload
    checkUpdatePayload pm _mbUpdatePayload

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

deriveSafeCopySimple 0 'base ''MainExtraBodyData
deriveSafeCopySimple 0 'base ''MainExtraHeaderData


----------------------------------------------------------------------------
-- MainBody lenses
----------------------------------------------------------------------------

makeLenses 'MainBody

-- | Lens for transaction tree in main block body.
mbTxs :: Lens' MainBody ([Tx])
mbTxs = mbTxPayload . txpTxs

-- | Lens for witness list in main block body.
mbWitnesses :: Lens' MainBody [TxWitness]
mbWitnesses = mbTxPayload . txpWitnesses


----------------------------------------------------------------------------
-- MainExtra lenses
----------------------------------------------------------------------------

makeLenses ''MainExtraHeaderData
makeLenses ''MainExtraBodyData

instance HasBlockVersion MainExtraHeaderData where
    blockVersionL = mehBlockVersion

instance HasSoftwareVersion MainExtraHeaderData where
    softwareVersionL = mehSoftwareVersion
