{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeFamilies     #-}

-- | Core functions from SSC.

module Pos.Sinbin.Ssc.Base
       ( isCommitmentId
       , isCommitmentIdx
       , isCommitmentIdExplicit
       , isOpeningId
       , isOpeningIdx
       , isOpeningIdExplicit
       , isSharesId
       , isSharesIdx
       , isSharesIdExplicit
       , mkSignedCommitment
       ) where

import           Universum

import           Data.Ix (inRange)

import           Pos.Core (EpochIndex, LocalSlotIndex, SlotCount, SlotId (..),
                     pcEpochSlots, unsafeMkLocalSlotIndexExplicit)
import           Pos.Core.Configuration (HasProtocolConstants,
                     protocolConstants)
import           Pos.Core.ProtocolConstants (ProtocolConstants (..),
                     pcSlotSecurityParam)
import           Pos.Core.Ssc (Commitment (..), SignedCommitment)
import           Pos.Crypto (ProtocolMagic, SecretKey, SignTag (..), sign,
                     toPublic)


toLocalSlotIndex :: ProtocolConstants -> SlotCount -> LocalSlotIndex
toLocalSlotIndex pc = unsafeMkLocalSlotIndexExplicit (pcEpochSlots pc) . fromIntegral

isCommitmentIdxExplicit :: ProtocolConstants -> LocalSlotIndex -> Bool
isCommitmentIdxExplicit pc =
    inRange (toLocalSlotIndex pc 0,
             toLocalSlotIndex pc (pcSlotSecurityParam pc - 1))

isCommitmentIdx :: HasProtocolConstants => LocalSlotIndex -> Bool
isCommitmentIdx = isCommitmentIdxExplicit protocolConstants

isOpeningIdxExplicit :: ProtocolConstants -> LocalSlotIndex -> Bool
isOpeningIdxExplicit pc =
    inRange (toLocalSlotIndex pc (2 * pcSlotSecurityParam pc),
             toLocalSlotIndex pc (3 * pcSlotSecurityParam pc - 1))

isOpeningIdx :: HasProtocolConstants => LocalSlotIndex -> Bool
isOpeningIdx = isOpeningIdxExplicit protocolConstants

isOpeningIdExplicit :: ProtocolConstants -> SlotId -> Bool
isOpeningIdExplicit pc = isOpeningIdxExplicit pc . siSlot

isSharesIdxExplicit :: ProtocolConstants -> LocalSlotIndex -> Bool
isSharesIdxExplicit pc =
    inRange (toLocalSlotIndex pc (4 * pcSlotSecurityParam pc),
             toLocalSlotIndex pc (5 * pcSlotSecurityParam pc - 1))

isSharesIdx :: HasProtocolConstants => LocalSlotIndex -> Bool
isSharesIdx = isSharesIdxExplicit protocolConstants

isCommitmentIdExplicit :: ProtocolConstants -> SlotId -> Bool
isCommitmentIdExplicit pc = isCommitmentIdxExplicit pc . siSlot

isCommitmentId :: HasProtocolConstants => SlotId -> Bool
isCommitmentId = isCommitmentIdExplicit protocolConstants

isOpeningId :: HasProtocolConstants => SlotId -> Bool
isOpeningId = isOpeningIdExplicit protocolConstants

isSharesIdExplicit :: ProtocolConstants -> SlotId -> Bool
isSharesIdExplicit pc = isSharesIdxExplicit pc . siSlot

isSharesId :: HasProtocolConstants => SlotId -> Bool
isSharesId = isSharesIdExplicit protocolConstants

-- | Make signed commitment from commitment and epoch index using secret key.
mkSignedCommitment
    :: ProtocolMagic -> SecretKey -> EpochIndex -> Commitment -> SignedCommitment
mkSignedCommitment pm sk i c = (toPublic sk, c, sign pm SignCommitment sk (i, c))
