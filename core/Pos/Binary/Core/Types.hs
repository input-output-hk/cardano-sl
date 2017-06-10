module Pos.Binary.Core.Types () where

import           Universum

import           Pos.Binary.Class        (Bi (..), Size (..), UnsignedVarInt (..),
                                          combineSize, label, putWord8, sizeAddField,
                                          sizeOf)
import qualified Pos.Binary.Core.Coin    as BinCoin
import           Pos.Binary.Core.Script  ()
import           Pos.Binary.Core.Version ()
import qualified Pos.Core.Types          as T
import qualified Pos.Data.Attributes     as A
import           Pos.Util.Util           (eitherToFail)

-- kind of boilerplate, but anyway that's what it was made for --
-- verbosity and clarity

instance Bi T.Timestamp where
    get = label "Timestamp" $ fromInteger <$> get
    put = put . toInteger
    size = sizeOf toInteger

instance Bi T.EpochIndex where
    get = label "EpochIndex" $ T.EpochIndex . getUnsignedVarInt <$> get
    put (T.EpochIndex c) = put (UnsignedVarInt c)
    size = sizeOf (UnsignedVarInt . T.getEpochIndex)

instance Bi (A.Attributes ()) where
    size = VarSize $ A.sizeAttributes (\() -> [])
    get = label "Attributes" $
        A.getAttributes (\_ () -> Nothing) (Just (128 * 1024 * 1024)) ()
    put = A.putAttributes (\() -> [])

instance Bi T.Coin where
    size = VarSize BinCoin.size
    put = mapM_ putWord8 . BinCoin.encode
    get = label "Coin" $ BinCoin.decode

instance Bi T.CoinPortion where
    put = put . T.getCoinPortion
    get = label "CoinPortion" $ get >>= T.mkCoinPortion
    size = sizeOf T.getCoinPortion

instance Bi T.LocalSlotIndex where
    get =
        label "LocalSlotIndex" $
        eitherToFail . T.mkLocalSlotIndex . getUnsignedVarInt =<< get
    put (T.getSlotIndex -> c) = put (UnsignedVarInt c)
    size = sizeOf (UnsignedVarInt . T.getSlotIndex)

instance Bi T.SlotId where
    get = label "SlotId" $ do
        siEpoch <- get
        siSlot <- get
        return $ T.SlotId {..}
    put (T.SlotId e s) = put e >> put s
    size = combineSize (T.siEpoch, T.siSlot)

instance Bi T.EpochOrSlot where
    put (T.EpochOrSlot x) = put x
    get = T.EpochOrSlot <$> get
    size = sizeOf T.unEpochOrSlot

-- serialized as vector of TxInWitness
--instance Bi T.TxWitness where

instance Bi T.SharedSeed where
    put (T.SharedSeed bs) = put bs
    get = label "SharedSeed" $ T.SharedSeed <$> get
    size = sizeOf T.getSharedSeed

instance Bi T.ChainDifficulty where
    get = label "ChainDifficulty" $
          T.ChainDifficulty . getUnsignedVarInt <$> get
    put (T.ChainDifficulty c) = put (UnsignedVarInt c)
    size = sizeOf (UnsignedVarInt . T.getChainDifficulty)

instance Bi T.BlockVersionData where
    get = label "BlockVersionData" $ do
        bvdScriptVersion     <- get
        bvdSlotDuration      <- get
        bvdMaxBlockSize      <- get
        bvdMaxHeaderSize     <- get
        bvdMaxTxSize         <- get
        bvdMaxProposalSize   <- get
        bvdMpcThd            <- get
        bvdHeavyDelThd       <- get
        bvdUpdateVoteThd     <- get
        bvdUpdateProposalThd <- get
        bvdUpdateImplicit    <- get
        bvdUpdateSoftforkThd <- get
        return $ T.BlockVersionData {..}
    put T.BlockVersionData {..} = do
        put bvdScriptVersion
        put bvdSlotDuration
        put bvdMaxBlockSize
        put bvdMaxHeaderSize
        put bvdMaxTxSize
        put bvdMaxProposalSize
        put bvdMpcThd
        put bvdHeavyDelThd
        put bvdUpdateVoteThd
        put bvdUpdateProposalThd
        put bvdUpdateImplicit
        put bvdUpdateSoftforkThd
    size = ConstSize 0
             `sizeAddField` T.bvdScriptVersion
             `sizeAddField` T.bvdSlotDuration
             `sizeAddField` T.bvdMaxBlockSize
             `sizeAddField` T.bvdMaxHeaderSize
             `sizeAddField` T.bvdMaxTxSize
             `sizeAddField` T.bvdMaxProposalSize
             `sizeAddField` T.bvdMpcThd
             `sizeAddField` T.bvdHeavyDelThd
             `sizeAddField` T.bvdUpdateVoteThd
             `sizeAddField` T.bvdUpdateProposalThd
             `sizeAddField` T.bvdUpdateImplicit
             `sizeAddField` T.bvdUpdateSoftforkThd
