{-# LANGUAGE UndecidableInstances #-}

module Pos.Binary.Block.Types
       (
       ) where

import           Data.Binary.Get           (getInt32be, getWord8, label)
import           Data.Binary.Put           (putInt32be, putWord8)
import qualified Data.Text                 as Text
import           Universum

import           Pos.Binary.Class          (Bi (..))
import           Pos.Binary.Core           ()
import           Pos.Binary.Txp            ()
import           Pos.Constants             (protocolMagic)
import qualified Pos.Core.Block            as T
import qualified Pos.Core.Types            as T
import           Pos.Ssc.Class.Types       (Ssc (..))
import qualified Pos.Types.Block.Instances as T
import qualified Pos.Types.Block.Types     as T
import           Pos.Update.Core.Types     (UpdatePayload)

----------------------------------------------------------------------------
-- Generic block header
----------------------------------------------------------------------------

-- | This instance required only for Arbitrary instance of HeaderHash
-- due to @instance Bi a => Hash a@.
instance Bi T.BlockHeaderStub where
    put _ = panic "somebody tried to binary put BlockHeaderStub"
    get   = panic "somebody tried to binary get BlockHeaderStub"

instance ( Bi (T.BHeaderHash b)
         , Bi (T.BodyProof b)
         , Bi (T.ConsensusData b)
         , Bi (T.ExtraHeaderData b)
         ) =>
         Bi (T.GenericBlockHeader b) where
    put T.GenericBlockHeader{..} = do
        putInt32be protocolMagic
        put _gbhPrevBlock
        put _gbhBodyProof
        put _gbhConsensus
        put _gbhExtra
    get =
        label "GenericBlockHeader" $ do
        blockMagic <- getInt32be
        when (blockMagic /= protocolMagic) $
            fail $ "GenericBlockHeader failed with wrong magic: " <> show blockMagic
        T.GenericBlockHeader <$> get <*> get <*> get <*> get

instance ( Bi (T.BHeaderHash b)
         , Bi (T.BodyProof b)
         , Bi (T.ConsensusData b)
         , Bi (T.ExtraHeaderData b)
         , Bi (T.Body b)
         , Bi (T.ExtraBodyData b)
         , T.Blockchain b
         ) =>
         Bi (T.GenericBlock b) where
    put T.GenericBlock {..} = do
        put _gbHeader
        put _gbBody
        put _gbExtra
    get =
        label "GenericBlock" $ do
            _gbHeader <- get
            _gbBody <- get
            _gbExtra <- get
            unless (T.checkBodyProof _gbBody (T._gbhBodyProof _gbHeader)) $
                fail "get@GenericBlock: incorrect proof of body"
            let gb = T.GenericBlock {..}
            case T.verifyBBlock gb of
                Left err -> fail $ Text.unpack $ "get@GenericBlock failed: " <> err
                Right _  -> pass
            return gb

----------------------------------------------------------------------------
-- MainBlock
----------------------------------------------------------------------------

instance Ssc ssc =>
         Bi (T.BodyProof (T.MainBlockchain ssc)) where
    put T.MainProof {..} = do
        put mpTxProof
        put mpMpcProof
        put mpProxySKsProof
        put mpUpdateProof
    get = label "MainProof" $ T.MainProof <$> get <*> get <*> get <*> get

instance Bi (T.BlockSignature ssc) where
    put (T.BlockSignature sig)             = putWord8 0 >> put sig
    put (T.BlockPSignatureEpoch proxySig)  = putWord8 1 >> put proxySig
    put (T.BlockPSignatureSimple proxySig) = putWord8 2 >> put proxySig
    get = label "BlockSignature" $ getWord8 >>= \case
        0 -> T.BlockSignature <$> get
        1 -> T.BlockPSignatureEpoch <$> get
        2 -> T.BlockPSignatureSimple <$> get
        t -> fail $ "get@BlockSignature: unknown tag: " <> show t

instance Bi (T.ConsensusData (T.MainBlockchain ssc)) where
    put T.MainConsensusData{..} = do
        put _mcdSlot
        put _mcdLeaderKey
        put _mcdDifficulty
        put _mcdSignature
    get = label "MainConsensusData" $ T.MainConsensusData <$> get <*> get <*> get <*> get

instance (Ssc ssc, Bi UpdatePayload) => Bi (T.Body (T.MainBlockchain ssc)) where
    put T.MainBody{..} = do
        put _mbTxPayload
        put _mbMpc
        put _mbProxySKs
        put _mbUpdatePayload
    get = label "MainBody" $ do
        _mbTxPayload           <- get
        _mbMpc                 <- get
        _mbProxySKs            <- get
        _mbUpdatePayload       <- get
        return T.MainBody{..}

instance Bi T.MainExtraHeaderData where
    put T.MainExtraHeaderData {..} =  put _mehBlockVersion
                                   *> put _mehSoftwareVersion
                                   *> put _mehAttributes
    get = label "MainExtraHeaderData" $ T.MainExtraHeaderData <$> get <*> get <*> get

instance Bi T.MainExtraBodyData where
   put T.MainExtraBodyData{..} = put _mebAttributes
   get = label "MainExtraBodyData" $ T.MainExtraBodyData <$> get

----------------------------------------------------------------------------
-- GenesisBlock
----------------------------------------------------------------------------

instance Bi (T.BodyProof (T.GenesisBlockchain ssc)) where
    put (T.GenesisProof h) = put h
    get = label "GenesisProof" $ T.GenesisProof <$> get

instance Bi (T.ConsensusData (T.GenesisBlockchain ssc)) where
    put T.GenesisConsensusData{..} = put _gcdEpoch >> put _gcdDifficulty
    get = label "GenesisConsensusData" $ T.GenesisConsensusData <$> get <*> get

instance Bi (T.Body (T.GenesisBlockchain ssc)) where
    put (T.GenesisBody leaders) = put leaders
    get = label "GenesisBody" $ T.GenesisBody <$> get
