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
import qualified Pos.Binary.Cbor            as Cbor
import qualified Pos.Core.Fee               as T
import qualified Pos.Core.Types             as T
import qualified Pos.Data.Attributes        as A
import           Pos.Util.Util              (eitherToFail)

-- kind of boilerplate, but anyway that's what it was made for --
-- verbosity and clarity

instance Bi T.Timestamp where
    sizeNPut = labelS "Timestamp" $ putField toInteger
    get = label "Timestamp" $ fromInteger <$> get

instance Cbor.Bi T.Timestamp where
  encode (T.Timestamp ms) = Cbor.encode . toInteger $ ms
  decode = T.Timestamp . fromIntegral <$> Cbor.decode @Integer

instance Bi T.EpochIndex where
    sizeNPut = labelS "EpochIndex" $ putField (UnsignedVarInt . T.getEpochIndex)
    get = label "EpochIndex" $ T.EpochIndex . getUnsignedVarInt <$> get

instance Cbor.Bi T.EpochIndex where
  encode (T.EpochIndex epoch) = Cbor.encode epoch
  decode = T.EpochIndex <$> Cbor.decode

instance Bi (A.Attributes ()) where
    size = VarSize $ A.sizeAttributes (\() -> [])
    get = label "Attributes" $
        A.getAttributes (\_ () -> Nothing) (Just (128 * 1024 * 1024)) ()
    put = labelP "Attributes" . A.putAttributes (\() -> [])

-- Stubbed in preparation to refactoring to better fit it into the CBOR model.
instance Cbor.Bi (A.Attributes ()) where
  encode = error "Attributes () encode: Unimplemented."
  decode = fail  "Attributes () decode: Unimplemented."

instance Bi T.Coin where
    size = VarSize BinCoin.size
    put = labelP "Coin" . mapM_ putWord8 . BinCoin.encode
    get = label "Coin" $ BinCoin.decode

instance Cbor.Bi T.Coin where
  encode = Cbor.encode . T.unsafeGetCoin
  decode = T.mkCoin <$> Cbor.decode

instance Bi T.CoinPortion where
    sizeNPut = labelS "CoinPortion" $ putField T.getCoinPortion
    get = label "CoinPortion" $ get >>= T.mkCoinPortion

instance Cbor.Bi T.CoinPortion where
  encode = Cbor.encode . T.getCoinPortion
  decode = do
    word64 <- Cbor.decode @Word64
    case T.mkCoinPortion word64 of
      Left err          -> fail err
      Right coinPortion -> return coinPortion

instance Bi T.LocalSlotIndex where
    sizeNPut = labelS "LocalSlotIndex" $
        putField (UnsignedVarInt . T.getSlotIndex)
    get =
        label "LocalSlotIndex" $
        eitherToFail . T.mkLocalSlotIndex . getUnsignedVarInt =<< get

instance Cbor.Bi T.LocalSlotIndex where
  encode = Cbor.encode . T.getSlotIndex
  decode = do
    word16 <- Cbor.decode @Word16
    case T.mkLocalSlotIndex word16 of
      Left err        -> fail (toString err)
      Right slotIndex -> return slotIndex

deriveSimpleBi ''T.SlotId [
    Cons 'T.SlotId [
        Field [| T.siEpoch :: T.EpochIndex     |],
        Field [| T.siSlot  :: T.LocalSlotIndex |]
    ]]

Cbor.deriveSimpleBi ''T.SlotId [
    Cbor.Cons 'T.SlotId [
        Cbor.Field [| T.siEpoch :: T.EpochIndex     |],
        Cbor.Field [| T.siSlot  :: T.LocalSlotIndex |]
    ]]

instance Bi T.EpochOrSlot where
    sizeNPut = labelS "EpochOrSlot" $ putField T.unEpochOrSlot
    get = label "EpochOrSlot" $ T.EpochOrSlot <$> get

instance Cbor.Bi T.EpochOrSlot where
  encode (T.EpochOrSlot e) = Cbor.encode e
  decode = T.EpochOrSlot <$> Cbor.decode @(Either T.EpochIndex T.SlotId)

-- serialized as vector of TxInWitness
--instance Bi T.TxWitness where

deriveSimpleBi ''T.SharedSeed [
    Cons 'T.SharedSeed [
        Field [| T.getSharedSeed :: ByteString |]
    ]]

Cbor.deriveSimpleBi ''T.SharedSeed [
    Cbor.Cons 'T.SharedSeed [
        Cbor.Field [| T.getSharedSeed :: ByteString |]
    ]]

instance Bi T.ChainDifficulty where
    sizeNPut = labelS "ChainDifficulty" $ putField (UnsignedVarInt . T.getChainDifficulty)
    get = label "ChainDifficulty" $
          T.ChainDifficulty . getUnsignedVarInt <$> get

instance Cbor.Bi T.ChainDifficulty where
  encode (T.ChainDifficulty word64) = Cbor.encode word64
  decode = T.ChainDifficulty <$> Cbor.decode @Word64

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

Cbor.deriveSimpleBi ''T.BlockVersionData [
    Cbor.Cons 'T.BlockVersionData [
        Cbor.Field [| T.bvdScriptVersion     :: T.ScriptVersion |],
        Cbor.Field [| T.bvdSlotDuration      :: Millisecond     |],
        Cbor.Field [| T.bvdMaxBlockSize      :: Byte            |],
        Cbor.Field [| T.bvdMaxHeaderSize     :: Byte            |],
        Cbor.Field [| T.bvdMaxTxSize         :: Byte            |],
        Cbor.Field [| T.bvdMaxProposalSize   :: Byte            |],
        Cbor.Field [| T.bvdMpcThd            :: T.CoinPortion   |],
        Cbor.Field [| T.bvdHeavyDelThd       :: T.CoinPortion   |],
        Cbor.Field [| T.bvdUpdateVoteThd     :: T.CoinPortion   |],
        Cbor.Field [| T.bvdUpdateProposalThd :: T.CoinPortion   |],
        Cbor.Field [| T.bvdUpdateImplicit    :: T.FlatSlotId    |],
        Cbor.Field [| T.bvdUpdateSoftforkThd :: T.CoinPortion   |],
        Cbor.Field [| T.bvdTxFeePolicy       :: T.TxFeePolicy   |]
    ]]
