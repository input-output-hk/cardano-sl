module Pos.Core.Slotting.LocalSlotIndex
       ( LocalSlotIndex (..)

       , mkLocalSlotIndex
       , mkLocalSlotIndexExplicit
       , mkLocalSlotIndexThrow_
       , addLocalSlotIndex

       , localSlotIndexMinBound
       , localSlotIndexMaxBound
       , localSlotIndices

       , unsafeMkLocalSlotIndex
       , unsafeMkLocalSlotIndexExplicit
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import           Data.Ix (Ix)
import           System.Random (Random (..))

import           Pos.Core.Configuration.Protocol (HasProtocolConstants, epochSlots,
                                                  protocolConstants)
import           Pos.Core.ProtocolConstants (ProtocolConstants, pcEpochSlots)
import           Pos.Util.Util (leftToPanic)

import           Pos.Core.Slotting.SlotCount (SlotCount)

-- | Index of slot inside a concrete epoch.
newtype LocalSlotIndex = UnsafeLocalSlotIndex
    { getSlotIndex :: Word16
    } deriving (Show, Eq, Ord, Ix, Generic, Hashable, Buildable, Typeable, NFData)

instance HasProtocolConstants => Enum LocalSlotIndex where
    toEnum i | i >= fromIntegral epochSlots = error "toEnum @LocalSlotIndex: greater than maxBound"
             | i < 0 = error "toEnum @LocalSlotIndex: less than minBound"
             | otherwise = UnsafeLocalSlotIndex (fromIntegral i)
    fromEnum = fromIntegral . getSlotIndex

instance HasProtocolConstants => Random LocalSlotIndex where
    random = randomR (minBound, maxBound)
    randomR (UnsafeLocalSlotIndex lo, UnsafeLocalSlotIndex hi) g =
        let (r, g') = randomR (lo, hi) g
        in  (UnsafeLocalSlotIndex r, g')

instance HasProtocolConstants => Bounded LocalSlotIndex where
    minBound = UnsafeLocalSlotIndex 0
    maxBound = UnsafeLocalSlotIndex (fromIntegral epochSlots - 1)

localSlotIndexMinBound :: LocalSlotIndex
localSlotIndexMinBound = UnsafeLocalSlotIndex 0

localSlotIndexMaxBound :: ProtocolConstants -> LocalSlotIndex
localSlotIndexMaxBound pc = UnsafeLocalSlotIndex (fromIntegral (pcEpochSlots pc) - 1)

-- | All local slot indices for the given number of slots in epoch, in ascending
-- order.
localSlotIndices :: SlotCount -> [LocalSlotIndex]
localSlotIndices slotsInEpoch = fmap UnsafeLocalSlotIndex [0..upperBound]
  where
    upperBound = fromIntegral slotsInEpoch - 1

mkLocalSlotIndex_ :: SlotCount -> Word16 -> Maybe LocalSlotIndex
mkLocalSlotIndex_ es idx
    | idx < fromIntegral es = Just (UnsafeLocalSlotIndex idx)
    | otherwise = Nothing

mkLocalSlotIndexThrow_ :: MonadError Text m => SlotCount -> Word16 -> m LocalSlotIndex
mkLocalSlotIndexThrow_ es idx = case mkLocalSlotIndex_ es idx of
    Just it -> pure it
    Nothing -> throwError $
        "local slot is greater than or equal to the number of slots in epoch: " <>
        show idx

mkLocalSlotIndex :: (HasProtocolConstants, MonadError Text m) => Word16 -> m LocalSlotIndex
mkLocalSlotIndex = mkLocalSlotIndexThrow_ epochSlots

mkLocalSlotIndexExplicit :: MonadError Text m => ProtocolConstants -> Word16 -> m LocalSlotIndex
mkLocalSlotIndexExplicit pc = mkLocalSlotIndexThrow_ (pcEpochSlots pc)

-- | Shift slot index by given amount, and return 'Nothing' if it has
-- overflowed past 'epochSlots'.
addLocalSlotIndex :: HasProtocolConstants => SlotCount -> LocalSlotIndex -> Maybe LocalSlotIndex
addLocalSlotIndex x (UnsafeLocalSlotIndex i)
    | s < fromIntegral epochSlots = Just (UnsafeLocalSlotIndex (fromIntegral s))
    | otherwise      = Nothing
  where
    s :: Word64
    s = fromIntegral x + fromIntegral i

-- | Unsafe constructor of 'LocalSlotIndex'.
unsafeMkLocalSlotIndex :: HasProtocolConstants => Word16 -> LocalSlotIndex
unsafeMkLocalSlotIndex = unsafeMkLocalSlotIndexExplicit protocolConstants

unsafeMkLocalSlotIndexExplicit :: ProtocolConstants -> Word16 -> LocalSlotIndex
unsafeMkLocalSlotIndexExplicit pc =
    leftToPanic "unsafeMkLocalSlotIndex failed: " . mkLocalSlotIndexExplicit pc
