module Pos.Core.Slotting.LocalSlotIndex
       ( LocalSlotIndex (..)

       , mkLocalSlotIndex
       , mkLocalSlotIndexExplicit
       , mkLocalSlotIndexThrow_
       , addLocalSlotIndex

       , localSlotIndexToEnum
       , localSlotIndexFromEnum
       , localSlotIndexSucc
       , localSlotIndexPred

       , localSlotIndexMinBound
       , localSlotIndexMaxBound
       , localSlotIndexMaxBoundExplicit
       , localSlotIndices

       , unsafeMkLocalSlotIndexExplicit
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Ix (Ix)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           System.Random (Random (..))

import           Pos.Binary.Class (Bi (..))
import           Pos.Core.Configuration.Protocol (HasProtocolConstants,
                     epochSlots)
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

instance Bi LocalSlotIndex where
    encode = encode . getSlotIndex
    decode = UnsafeLocalSlotIndex <$> decode

localSlotIndexMinBound :: LocalSlotIndex
localSlotIndexMinBound = UnsafeLocalSlotIndex 0

localSlotIndexMaxBound :: ProtocolConstants -> LocalSlotIndex
localSlotIndexMaxBound pc = UnsafeLocalSlotIndex (fromIntegral (pcEpochSlots pc) - 1)

localSlotIndexMaxBoundExplicit :: SlotCount -> LocalSlotIndex
localSlotIndexMaxBoundExplicit es = UnsafeLocalSlotIndex (fromIntegral es - 1)

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

mkLocalSlotIndexExplicit :: MonadError Text m => SlotCount -> Word16 -> m LocalSlotIndex
mkLocalSlotIndexExplicit es = mkLocalSlotIndexThrow_ es

-- | Shift slot index by given amount, and return 'Nothing' if it has
-- overflowed past 'epochSlots'.
addLocalSlotIndex :: HasProtocolConstants => SlotCount -> LocalSlotIndex -> Maybe LocalSlotIndex
addLocalSlotIndex x (UnsafeLocalSlotIndex i)
    | s < fromIntegral epochSlots = Just (UnsafeLocalSlotIndex (fromIntegral s))
    | otherwise      = Nothing
  where
    s :: Word64
    s = fromIntegral x + fromIntegral i

unsafeMkLocalSlotIndexExplicit :: SlotCount -> Word16 -> LocalSlotIndex
unsafeMkLocalSlotIndexExplicit es =
    leftToPanic "unsafeMkLocalSlotIndex failed: " . mkLocalSlotIndexExplicit es

-- -----------------------------------------------------------------------------
-- LocalSlotIndex used to have an 'Enum' instance, but the pending removal of
-- 'HasProtocolConstants' means that is no longer possible. Instead we use
-- functions.

localSlotIndexToEnum :: SlotCount -> Int -> LocalSlotIndex
localSlotIndexToEnum es i
    | i >= fromIntegral es = error
        "localSlotIndexToEnum: greater than maxBound"
    | i < 0 = error "localSlotIndexToEnum: less than minBound"
    | otherwise = UnsafeLocalSlotIndex (fromIntegral i)

localSlotIndexFromEnum :: LocalSlotIndex -> Int
localSlotIndexFromEnum = fromIntegral . getSlotIndex

localSlotIndexSucc :: SlotCount -> LocalSlotIndex -> LocalSlotIndex
localSlotIndexSucc es =
    localSlotIndexToEnum es . (+ 1) . localSlotIndexFromEnum

localSlotIndexPred :: SlotCount -> LocalSlotIndex -> LocalSlotIndex
localSlotIndexPred es =
    localSlotIndexToEnum es . subtract 1 . localSlotIndexFromEnum

-- -----------------------------------------------------------------------------
-- TH derived instances at the end of the file.

deriveJSON defaultOptions ''LocalSlotIndex

deriveSafeCopySimple 0 'base ''LocalSlotIndex
