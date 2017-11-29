module Pos.Binary.Core.Types () where

import           Universum

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), deriveSimpleBi,
                                   deriveSimpleBiCxt)
import           Pos.Binary.Core.Coin ()
import           Pos.Binary.Core.Fee ()
import           Pos.Binary.Core.Script ()
import           Pos.Core.Configuration.Protocol (HasProtocolConstants)
import qualified Pos.Core.Slotting as T
import qualified Pos.Core.Types as T
import qualified Pos.Data.Attributes as A
import           Pos.Util.Orphans ()

-- kind of boilerplate, but anyway that's what it was made for --
-- verbosity and clarity

instance Bi T.Timestamp where
    encode (T.Timestamp ms) = encode . toInteger $ ms
    decode = T.Timestamp . fromIntegral <$> decode @Integer

instance Bi T.TimeDiff where
    encode = encode . toInteger
    decode = fromInteger <$> decode

instance Bi T.EpochIndex where
    encode (T.EpochIndex epoch) = encode epoch
    decode = T.EpochIndex <$> decode

instance Bi (A.Attributes ()) where
    encode = A.encodeAttributes []
    decode = A.decodeAttributes () $ \_ _ _ -> pure Nothing

instance Bi T.CoinPortion where
    encode = encode . T.getCoinPortion
    decode =
        T.mkCoinPortion <$> (decode @Word64) >>= \case
            Left err          -> fail err
            Right coinPortion -> return coinPortion

instance HasProtocolConstants => Bi T.LocalSlotIndex where
    encode = encode . T.getSlotIndex
    decode = do
        word16 <- decode @Word16
        case T.mkLocalSlotIndex word16 of
            Left err        -> fail ("decode@LocalSlotIndex: " <> toString err)
            Right slotIndex -> return slotIndex

deriveSimpleBiCxt [t| HasProtocolConstants |] ''T.SlotId [
    Cons 'T.SlotId [
        Field [| T.siEpoch :: T.EpochIndex     |],
        Field [| T.siSlot  :: T.LocalSlotIndex |]
    ]]

instance HasProtocolConstants => Bi T.EpochOrSlot where
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
