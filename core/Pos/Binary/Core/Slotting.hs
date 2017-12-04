-- | Binary serialization of slotting types.

module Pos.Binary.Core.Slotting
       (
       ) where

import           Universum

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), deriveSimpleBiCxt)
import           Pos.Core.Configuration.Protocol (HasProtocolConstants)
import qualified Pos.Core.Slotting as T

instance Bi T.Timestamp where
    encode (T.Timestamp ms) = encode . toInteger $ ms
    decode = T.Timestamp . fromIntegral <$> decode @Integer

instance Bi T.TimeDiff where
    encode = encode . toInteger
    decode = fromInteger <$> decode

instance Bi T.EpochIndex where
    encode (T.EpochIndex epoch) = encode epoch
    decode = T.EpochIndex <$> decode

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
