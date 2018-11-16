{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Pos.Core.Slotting.EpochOrSlot
       ( EpochOrSlot (..)
       , HasEpochOrSlot (..)

       , epochOrSlotToEnum
       , epochOrSlotFromEnum
       , epochOrSlotFromSlotId
       , epochOrSlotSucc
       , epochOrSlotPred
       , epochOrSlotEnumFromTo

       , epochOrSlotMinBound
       , epochOrSlotMaxBound

       , flattenEpochOrSlot

       , diffEpochOrSlot

       , epochOrSlot
       , epochOrSlotG
       , epochOrSlotToSlot
       ) where

import           Universum

import           Control.Lens (Getter, lens, to)
import           Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON, withObject,
                     (.:))
import           Data.SafeCopy (base, deriveSafeCopySimple)
import qualified Formatting.Buildable as Buildable
import           Pos.Util.Some (Some, applySome)

import           Pos.Binary.Class (Bi (..))
import           Pos.Util.Util (leftToPanic)

import           Pos.Core.Slotting.EpochIndex
import           Pos.Core.Slotting.LocalSlotIndex
import           Pos.Core.Slotting.SlotCount (SlotCount)
import           Pos.Core.Slotting.SlotId

-- | Represents SlotId or EpochIndex. Useful because genesis blocks
-- have only EpochIndex, while main blocks have SlotId.

-- This type is a pain in the neck. It would have been far better as a simple
-- sum type. Because this encoding is baked into the system (ie it has a Bi
-- instance that is either send over the network or stored on the blockchain)
-- its painful to change. The best we can do is provide helper functions to
-- minimise the pain.
newtype EpochOrSlot = EpochOrSlot
    { unEpochOrSlot :: Either EpochIndex SlotId
    } deriving (Show, Eq, Generic, NFData)

instance Ord EpochOrSlot where
    compare (EpochOrSlot e1) (EpochOrSlot e2) = case (e1,e2) of
        (Left s1, Left s2)                      -> compare s1 s2
        (Right s1, Left s2) | (siEpoch s1) < s2 -> LT
                            | otherwise         -> GT
        (Left s1, Right s2) | s1 > (siEpoch s2) -> GT
                            | otherwise         -> LT
        (Right s1, Right s2)
            | siEpoch s1 == siEpoch s2 -> siSlot s1 `compare` siSlot s2
            | otherwise -> siEpoch s1 `compare` siEpoch s2

instance Buildable EpochOrSlot where
    build = either Buildable.build Buildable.build . unEpochOrSlot

instance Bi EpochOrSlot where
    encode (EpochOrSlot e) = encode e
    decode = EpochOrSlot <$> decode @(Either EpochIndex SlotId)

instance FromJSON EpochOrSlot where
    parseJSON = withObject "EpochOrSlot" $ \v -> EpochOrSlot . Left . EpochIndex
                   <$> v .: "attribResrictEpoch"

instance ToJSON EpochOrSlot where
    toJSON (EpochOrSlot (Left eIndex)) = toJSON eIndex
    toJSON (EpochOrSlot (Right sId))   = toJSON sId

instance HasEpochIndex EpochOrSlot where
    epochIndexL = lens (epochOrSlot identity siEpoch) setter
      where
        setter :: EpochOrSlot -> EpochIndex -> EpochOrSlot
        setter (EpochOrSlot (Left _)) epoch = EpochOrSlot (Left epoch)
        setter (EpochOrSlot (Right slot)) epoch =
            EpochOrSlot (Right $ set epochIndexL epoch slot)

epochOrSlotToEnum :: SlotCount -> Int -> EpochOrSlot
epochOrSlotToEnum epochSlots x =
    let
        (fromIntegral -> epoch, fromIntegral -> slot) =
            x `divMod` (fromIntegral epochSlots + 1)
        slotIdx =
            leftToPanic "toEnum @EpochOrSlot" $ mkLocalSlotIndex epochSlots (slot - 1)
    in
        if | x < 0 -> error "toEnum @EpochOrSlot: Negative argument"
           | slot == 0 -> EpochOrSlot (Left epoch)
           | otherwise ->
             EpochOrSlot (Right SlotId {siSlot = slotIdx, siEpoch = epoch})

epochOrSlotFromEnum :: SlotCount -> EpochOrSlot -> Int
epochOrSlotFromEnum epochSlots = \case
    (EpochOrSlot (Left e)) ->
        let res = toInteger e * toInteger (epochSlots + 1)
            maxIntAsInteger = toInteger (maxBound :: Int)
        in if | res > maxIntAsInteger ->
                  error "fromEnum @EpochOrSlot: Argument larger than 'maxBound :: Int'"
              | otherwise -> fromIntegral res
    (EpochOrSlot (Right SlotId {..})) ->
        let res = toInteger (epochOrSlotFromEnum epochSlots (EpochOrSlot (Left siEpoch))) +
                  toInteger (getSlotIndex siSlot) +
                  1
            maxIntAsInteger = toInteger (maxBound :: Int)
        in if | res > maxIntAsInteger ->
                  error "fromEnum @EpochOrSlot: Argument larger than 'maxBound :: Int'"
              | otherwise -> fromIntegral res

epochOrSlotFromSlotId :: SlotId -> EpochOrSlot
epochOrSlotFromSlotId = EpochOrSlot . Right

epochOrSlotSucc :: SlotCount -> EpochOrSlot -> EpochOrSlot
epochOrSlotSucc epochSlots = \case
    (EpochOrSlot (Left e)) -> EpochOrSlot
        (Right SlotId {siEpoch = e, siSlot = localSlotIndexMinBound})
    e@(EpochOrSlot (Right si@SlotId {..}))
        | e == epochOrSlotMaxBound epochSlots -> error
            "succ@EpochOrSlot: maxBound"
        | siSlot == localSlotIndexMaxBound epochSlots -> EpochOrSlot
            (Left (siEpoch + 1))
        | otherwise -> EpochOrSlot
        $ Right si { siSlot = localSlotIndexSucc epochSlots siSlot }

epochOrSlotPred :: SlotCount -> EpochOrSlot -> EpochOrSlot
epochOrSlotPred epochSlots = \case
    eos@(EpochOrSlot (Left e))
        | eos == epochOrSlotMinBound -> error "epochOrSlotPred: minBound"
        | otherwise -> EpochOrSlot $ Right SlotId
            { siEpoch = e - 1
            , siSlot  = localSlotIndexMaxBound epochSlots
            }
    (EpochOrSlot (Right si@SlotId {..}))
        | siSlot == localSlotIndexMinBound -> EpochOrSlot (Left siEpoch)
        | otherwise -> EpochOrSlot
        $ Right si { siSlot = localSlotIndexPred epochSlots siSlot }

epochOrSlotEnumFromTo
    :: SlotCount -> EpochOrSlot -> EpochOrSlot -> [EpochOrSlot]
epochOrSlotEnumFromTo epochSlots x y = fmap
    (epochOrSlotToEnum epochSlots)
    [epochOrSlotFromEnum epochSlots x .. epochOrSlotFromEnum epochSlots y]

epochOrSlotMinBound :: EpochOrSlot
epochOrSlotMinBound = EpochOrSlot (Left (EpochIndex 0))

epochOrSlotMaxBound :: SlotCount -> EpochOrSlot
epochOrSlotMaxBound epochSlots = EpochOrSlot $ Right SlotId
    { siSlot  = localSlotIndexMaxBound epochSlots
    , siEpoch = maxBound
    }

class HasEpochOrSlot a where
    getEpochOrSlot :: a -> EpochOrSlot

instance HasEpochOrSlot (Some HasEpochOrSlot) where
    getEpochOrSlot = applySome getEpochOrSlot

epochOrSlotG :: HasEpochOrSlot a => Getter a EpochOrSlot
epochOrSlotG = to getEpochOrSlot

instance HasEpochOrSlot EpochIndex where
    getEpochOrSlot = EpochOrSlot . Left

instance HasEpochOrSlot SlotId where
    getEpochOrSlot = EpochOrSlot . Right

instance HasEpochOrSlot EpochOrSlot where
    getEpochOrSlot = identity

instance (HasEpochOrSlot a, HasEpochOrSlot b) =>
         HasEpochOrSlot (Either a b) where
    getEpochOrSlot = either getEpochOrSlot getEpochOrSlot

-- | Transforms some 'HasEpochOrSlot' to a single number.
flattenEpochOrSlot :: HasEpochOrSlot a => SlotCount -> a -> FlatSlotId
flattenEpochOrSlot epochSlots =
    epochOrSlot (flattenEpochIndex epochSlots) (flattenSlotId epochSlots)
        . getEpochOrSlot

-- | Distance (in slots) between two slots. The first slot is newer, the
-- second slot is older. An epoch is considered the same as the 0th slot of
-- that epoch.
--
-- If the difference is negative, the result will be 'Nothing'.
diffEpochOrSlot :: SlotCount -> EpochOrSlot -> EpochOrSlot -> Maybe SlotCount
diffEpochOrSlot epochSlots a b
    | a' < b'   = Nothing
    | otherwise = Just (fromInteger (a' - b'))
  where
    a' = toInteger (flattenEpochOrSlot epochSlots a)
    b' = toInteger (flattenEpochOrSlot epochSlots b)

-- | Apply one of the function depending on content of 'EpochOrSlot'.
epochOrSlot :: (EpochIndex -> a) -> (SlotId -> a) -> EpochOrSlot -> a
epochOrSlot f g = either f g . unEpochOrSlot

-- | Convert 'EpochOrSlot' to the corresponding slot. If slot is
-- stored, it's returned, otherwise 0-th slot from the stored epoch is
-- returned.
epochOrSlotToSlot :: EpochOrSlot -> SlotId
epochOrSlotToSlot = epochOrSlot (flip SlotId localSlotIndexMinBound) identity

-- -----------------------------------------------------------------------------
-- TH derived instances at the end of the file.

deriveSafeCopySimple 0 'base ''EpochOrSlot
