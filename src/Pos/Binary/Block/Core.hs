-- | Serialization of core types from 'Pos.Block'.

module Pos.Binary.Block.Core
       (
       ) where

import           Universum

import           Pos.Binary.Class             (Bi (..), getWord8, label, putWord8)
import           Pos.Binary.Core              ()
import           Pos.Binary.Txp               ()
import           Pos.Binary.Update            ()
import qualified Pos.Block.Core.Genesis.Chain as BC
import qualified Pos.Block.Core.Genesis.Types as BC
import qualified Pos.Block.Core.Main.Chain    as BC
import qualified Pos.Block.Core.Main.Types    as BC
import qualified Pos.Core.Block               as Core
import           Pos.Ssc.Class.Types          (Ssc (..))

----------------------------------------------------------------------------
-- MainBlock
----------------------------------------------------------------------------

instance Ssc ssc =>
         Bi (Core.BodyProof (BC.MainBlockchain ssc)) where
    put BC.MainProof {..} = do
        put mpTxProof
        put mpMpcProof
        put mpProxySKsProof
        put mpUpdateProof
    get = label "MainProof" $ BC.MainProof <$> get <*> get <*> get <*> get

instance Bi (BC.BlockSignature ssc) where
    put (BC.BlockSignature sig)            = putWord8 0 >> put sig
    put (BC.BlockPSignatureLight proxySig) = putWord8 1 >> put proxySig
    put (BC.BlockPSignatureHeavy proxySig) = putWord8 2 >> put proxySig
    get = label "BlockSignature" $ getWord8 >>= \case
        0 -> BC.BlockSignature <$> get
        1 -> BC.BlockPSignatureLight <$> get
        2 -> BC.BlockPSignatureHeavy <$> get
        t -> fail $ "get@BlockSignature: unknown tag: " <> show t

instance Bi (BC.ConsensusData (BC.MainBlockchain ssc)) where
    put BC.MainConsensusData{..} = do
        put _mcdSlot
        put _mcdLeaderKey
        put _mcdDifficulty
        put _mcdSignature
    get = label "MainConsensusData" $ BC.MainConsensusData <$> get <*> get <*> get <*> get

instance (Ssc ssc) => Bi (BC.Body (BC.MainBlockchain ssc)) where
    put BC.MainBody{..} = do
        put _mbTxPayload
        put _mbSscPayload
        put _mbDlgPayload
        put _mbUpdatePayload
    get = label "MainBody" $ do
        _mbTxPayload     <- get
        _mbSscPayload    <- get
        _mbDlgPayload    <- get
        _mbUpdatePayload <- get
        return BC.MainBody{..}

instance Bi BC.MainExtraHeaderData where
    put BC.MainExtraHeaderData {..} =  put _mehBlockVersion
                                   *> put _mehSoftwareVersion
                                   *> put _mehAttributes
    get = label "MainExtraHeaderData" $ BC.MainExtraHeaderData <$> get <*> get <*> get

instance Bi BC.MainExtraBodyData where
   put BC.MainExtraBodyData{..} = put _mebAttributes
   get = label "MainExtraBodyData" $ BC.MainExtraBodyData <$> get

instance Ssc ssc => Bi (BC.MainToSign ssc) where
    put BC.MainToSign {..} =
        put _msHeaderHash <>
        put _msBodyProof <>
        put _msSlot <>
        put _msChainDiff <>
        put _msExtraHeader
    get = label "MainToSign" $ BC.MainToSign <$> get <*> get <*> get <*> get <*> get

-- ----------------------------------------------------------------------------
-- -- GenesisBlock
-- ----------------------------------------------------------------------------

instance Bi BC.GenesisExtraHeaderData where
    put BC.GenesisExtraHeaderData {..} = put _gehAttributes
    get = label "GenesisExtraHeaderData" $ BC.GenesisExtraHeaderData <$> get

instance Bi BC.GenesisExtraBodyData where
    put BC.GenesisExtraBodyData {..} = put _gebAttributes
    get = label "GenesisExtraBodyData" $ BC.GenesisExtraBodyData <$> get

instance Bi (BC.BodyProof (BC.GenesisBlockchain ssc)) where
    put (BC.GenesisProof h) = put h
    get = label "GenesisProof" $ BC.GenesisProof <$> get

instance Bi (BC.ConsensusData (BC.GenesisBlockchain ssc)) where
    put BC.GenesisConsensusData{..} = put _gcdEpoch >> put _gcdDifficulty
    get = label "GenesisConsensusData" $ BC.GenesisConsensusData <$> get <*> get

instance Bi (BC.Body (BC.GenesisBlockchain ssc)) where
    put (BC.GenesisBody leaders) = put leaders
    get = label "GenesisBody" $ BC.GenesisBody <$> get
