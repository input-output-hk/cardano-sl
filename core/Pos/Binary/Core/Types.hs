module Pos.Binary.Core.Types () where

import           Universum

import           Data.Time.Units            (Millisecond)
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Binary.Class           (Bi (..), Cons (..), Field (..), deriveSimpleBi)
import           Pos.Binary.Core.Fee        ()
import           Pos.Binary.Core.Script     ()
import           Pos.Binary.Core.Version    ()
import qualified Pos.Binary.Core.Coin       as BinCoin
import qualified Pos.Core.Fee               as T
import qualified Pos.Core.Types             as T
import qualified Pos.Data.Attributes        as A

-- kind of boilerplate, but anyway that's what it was made for --
-- verbosity and clarity

instance Bi T.Timestamp where
  encode (T.Timestamp ms) = encode . toInteger $ ms
  decode = T.Timestamp . fromIntegral <$> decode @Integer

instance Bi T.EpochIndex where
  encode (T.EpochIndex epoch) = encode epoch
  decode = T.EpochIndex <$> decode

instance Bi (A.Attributes ()) where
  encode = A.encodeAttributes []
  decode = A.decodeAttributes () $ \_ _ _ -> Nothing

instance Bi T.Coin where
  encode = encode . BinCoin.encode
  decode = BinCoin.decode

instance Bi T.CoinPortion where
  encode = encode . T.getCoinPortion
  decode = do
    word64 <- decode @Word64
    case T.mkCoinPortion word64 of
      Left err          -> fail err
      Right coinPortion -> return coinPortion

instance Bi T.LocalSlotIndex where
  encode = encode . T.getSlotIndex
  decode = do
    word16 <- decode @Word16
    case T.mkLocalSlotIndex word16 of
      Left err        -> fail ("decode@LocalSlotIndex: " <> toString err)
      Right slotIndex -> return slotIndex

deriveSimpleBi ''T.SlotId [
    Cons 'T.SlotId [
        Field [| T.siEpoch :: T.EpochIndex     |],
        Field [| T.siSlot  :: T.LocalSlotIndex |]
    ]]

instance Bi T.EpochOrSlot where
  encode (T.EpochOrSlot e) = encode e
  decode = T.EpochOrSlot <$> decode @(Either T.EpochIndex T.SlotId)

instance Bi T.SlotCount where
    encode = encode . T.getSlotCount
    decode = T.SlotCount <$> decode

instance Bi T.BlockCount where
    encode = encode . T.getBlockCount
    decode = T.BlockCount <$> decode

-- serialized as vector of TxInWitness
--instance Bi T.TxWitness where

deriveSimpleBi ''T.SharedSeed [
    Cons 'T.SharedSeed [
        Field [| T.getSharedSeed :: ByteString |]
    ]]

deriveSimpleBi ''T.ChainDifficulty [
    Cons 'T.ChainDifficulty [
        Field [| T.getChainDifficulty :: T.BlockCount |]
    ]]

deriveSimpleBi ''T.SoftforkRule [
    Cons 'T.SoftforkRule [
        Field [| T.srInitThd      :: T.CoinPortion |],
        Field [| T.srMinThd       :: T.CoinPortion |],
        Field [| T.srThdDecrement :: T.CoinPortion |]
    ]]

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
        Field [| T.bvdSoftforkRule      :: T.SoftforkRule  |],
        Field [| T.bvdTxFeePolicy       :: T.TxFeePolicy   |],
        Field [| T.bvdUnlockStakeEpoch  :: T.EpochIndex    |]
    ]]
