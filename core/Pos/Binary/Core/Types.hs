module Pos.Binary.Core.Types () where

import           Universum

import           Pos.Binary.Class        (Bi (..), Size (..), UnsignedVarInt (..),
                                          appendField, label, putField, putWord8)
import qualified Pos.Binary.Core.Coin    as BinCoin
import           Pos.Binary.Core.Script  ()
import           Pos.Binary.Core.Version ()
import qualified Pos.Core.Types          as T
import qualified Pos.Data.Attributes     as A
import           Pos.Util.Util           (eitherToFail)

-- kind of boilerplate, but anyway that's what it was made for --
-- verbosity and clarity

instance Bi T.Timestamp where
    sizeNPut = putField toInteger
    get = label "Timestamp" $ fromInteger <$> get

instance Bi T.EpochIndex where
    sizeNPut = putField (UnsignedVarInt . T.getEpochIndex)
    get = label "EpochIndex" $ T.EpochIndex . getUnsignedVarInt <$> get

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
    sizeNPut = putField T.getCoinPortion
    get = label "CoinPortion" $ get >>= T.mkCoinPortion

instance Bi T.LocalSlotIndex where
    sizeNPut = putField (UnsignedVarInt . T.getSlotIndex)
    get =
        label "LocalSlotIndex" $
        eitherToFail . T.mkLocalSlotIndex . getUnsignedVarInt =<< get

instance Bi T.SlotId where
    sizeNPut = putField T.siEpoch
            <> putField T.siSlot
    get = label "SlotId" $ T.SlotId <$> get <*> get

instance Bi T.EpochOrSlot where
    sizeNPut = putField T.unEpochOrSlot
    get = T.EpochOrSlot <$> get

-- serialized as vector of TxInWitness
--instance Bi T.TxWitness where

instance Bi T.SharedSeed where
    sizeNPut = putField T.getSharedSeed
    get = label "SharedSeed" $ T.SharedSeed <$> get

instance Bi T.ChainDifficulty where
    sizeNPut = putField (UnsignedVarInt . T.getChainDifficulty)
    get = label "ChainDifficulty" $
          T.ChainDifficulty . getUnsignedVarInt <$> get

instance Bi T.BlockVersionData where
    sizeNPut = putField T.bvdScriptVersion
             `appendField` T.bvdSlotDuration
             `appendField` T.bvdMaxBlockSize
             `appendField` T.bvdMaxHeaderSize
             `appendField` T.bvdMaxTxSize
             `appendField` T.bvdMaxProposalSize
             `appendField` T.bvdMpcThd
             `appendField` T.bvdHeavyDelThd
             `appendField` T.bvdUpdateVoteThd
             `appendField` T.bvdUpdateProposalThd
             `appendField` T.bvdUpdateImplicit
             `appendField` T.bvdUpdateSoftforkThd
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
