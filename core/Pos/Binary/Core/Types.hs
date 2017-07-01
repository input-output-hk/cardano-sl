module Pos.Binary.Core.Types () where

import           Universum

import           Data.Time.Units            (Millisecond)
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Binary.Class           (Bi (..), Cons (..), Field (..), Size (..),
                                             UnsignedVarInt (..), deriveSimpleBi, label,
                                             labelP, labelS, putField, putWord8)
import qualified Pos.Binary.Core.Coin       as BinCoin
import           Pos.Binary.Core.Fee        ()
import           Pos.Binary.Core.Script     ()
import           Pos.Binary.Core.Version    ()
import qualified Pos.Core.Fee               as T
import qualified Pos.Core.Types             as T
import qualified Pos.Data.Attributes        as A
import           Pos.Util.Util              (eitherToFail)

-- kind of boilerplate, but anyway that's what it was made for --
-- verbosity and clarity

instance Bi T.Timestamp where
    sizeNPut = labelS "Timestamp" $ putField toInteger
    get = label "Timestamp" $ fromInteger <$> get

instance Bi T.EpochIndex where
    sizeNPut = labelS "EpochIndex" $ putField (UnsignedVarInt . T.getEpochIndex)
    get = label "EpochIndex" $ T.EpochIndex . getUnsignedVarInt <$> get

instance Bi (A.Attributes ()) where
    size = VarSize $ A.sizeAttributes (\() -> [])
    get = label "Attributes" $
        A.getAttributes (\_ () -> Nothing) (Just (128 * 1024 * 1024)) ()
    put = labelP "Attributes" . A.putAttributes (\() -> [])

instance Bi T.Coin where
    size = VarSize BinCoin.size
    put = labelP "Coin" . mapM_ putWord8 . BinCoin.encode
    get = label "Coin" $ BinCoin.decode

instance Bi T.CoinPortion where
    sizeNPut = labelS "CoinPortion" $ putField T.getCoinPortion
    get = label "CoinPortion" $ get >>= T.mkCoinPortion

instance Bi T.LocalSlotIndex where
    sizeNPut = labelS "LocalSlotIndex" $
        putField (UnsignedVarInt . T.getSlotIndex)
    get =
        label "LocalSlotIndex" $
        eitherToFail . T.mkLocalSlotIndex . getUnsignedVarInt =<< get

deriveSimpleBi ''T.SlotId [
    Cons 'T.SlotId [
        Field [| T.siEpoch :: T.EpochIndex     |],
        Field [| T.siSlot  :: T.LocalSlotIndex |]
    ]]

instance Bi T.EpochOrSlot where
    sizeNPut = labelS "EpochOrSlot" $ putField T.unEpochOrSlot
    get = label "EpochOrSlot" $ T.EpochOrSlot <$> get

-- serialized as vector of TxInWitness
--instance Bi T.TxWitness where

deriveSimpleBi ''T.SharedSeed [
    Cons 'T.SharedSeed [
        Field [| T.getSharedSeed :: ByteString |]
    ]]

instance Bi T.ChainDifficulty where
    sizeNPut = labelS "ChainDifficulty" $ putField (UnsignedVarInt . T.getChainDifficulty)
    get = label "ChainDifficulty" $
          T.ChainDifficulty . getUnsignedVarInt <$> get

deriveSimpleBi ''T.BlockVersionData [
    Cons 'T.BlockVersionData [
        Field [| T.bvdScriptVersion     :: T.ScriptVersion |],
        Field [| T.bvdSlotDuration      :: Millisecond     |],
        Field [| T.bvdMaxBlockSize      :: Byte            |],
        Field [| T.bvdMaxHeaderSize     :: Byte            |],
        Field [| T.bvdMaxTxSize         :: Byte            |],
        Field [| T.bvdMaxProposalSize   :: Byte            |],
        Field [| T.bvdMpcThd            :: T.CoinPortion   |],
        Field [| T.bvdHeavyDelThd       :: T.CoinPortion   |],
        Field [| T.bvdUpdateVoteThd     :: T.CoinPortion   |],
        Field [| T.bvdUpdateProposalThd :: T.CoinPortion   |],
        Field [| T.bvdUpdateImplicit    :: T.FlatSlotId    |],
        Field [| T.bvdUpdateSoftforkThd :: T.CoinPortion   |],
        Field [| T.bvdTxFeePolicy       :: T.TxFeePolicy   |]
    ]]
