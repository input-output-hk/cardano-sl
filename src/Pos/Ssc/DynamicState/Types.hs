{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | A “dynamic state” implementation of SSC. Nodes exchange commitments,
-- openings, and shares, and in the end arrive at a shared seed.
--
-- See https://eprint.iacr.org/2015/889.pdf (“A Provably Secure
-- Proof-of-Stake Blockchain Protocol”), section 4 for more details.

module Pos.Ssc.DynamicState.Types
       (
         -- * Instance types
         DSPayload(..)
       , DSProof(..)
       , DSMessage(..)
       , filterDSPayload
       , mkDSProof
       , verifyDSPayload

       -- * Lenses
       -- ** DSPayload
       , mdCommitments
       , mdOpenings
       , mdShares
       , mdVssCertificates

       -- * Utilities
       , hasCommitment
       , hasOpening
       , hasShares
       ) where

import           Control.Lens              (makeLenses, (^.))
import           Data.Binary               (Binary)
import qualified Data.HashMap.Strict       as HM
import           Data.MessagePack          (MessagePack)
import           Data.SafeCopy             (base, deriveSafeCopySimple)
import           Data.Text.Buildable       (Buildable (..))
import           Formatting                (bprint, (%))
import           Serokell.Util             (VerificationRes, listJson, verifyGeneric)
import           Universum

import           Pos.Crypto                (Hash, PublicKey, Share, hash)
import           Pos.Ssc.Class.Types       (SscTypes (SscPayload))
import           Pos.Ssc.DynamicState.Base (CommitmentsMap, Opening, OpeningsMap,
                                            SharesMap, SignedCommitment, VssCertificate,
                                            VssCertificatesMap, isCommitmentId,
                                            isOpeningId, isSharesId)
import           Pos.Types                 (MainBlockHeader, SlotId, headerSlot)

----------------------------------------------------------------------------
-- SscMessage
----------------------------------------------------------------------------

data DSMessage
    = DSCommitment !PublicKey
                   !SignedCommitment
    | DSOpening !PublicKey
                !Opening
    | DSShares !PublicKey
               (HashMap PublicKey Share)
    | DSVssCertificate !PublicKey
                       !VssCertificate
    deriving (Show)

deriveSafeCopySimple 0 'base ''DSMessage

----------------------------------------------------------------------------
-- SscPayload
----------------------------------------------------------------------------

-- | MPC-related content of main body.
data DSPayload = DSPayload
    { -- | Commitments are added during the first phase of epoch.
      _mdCommitments     :: !CommitmentsMap
      -- | Openings are added during the second phase of epoch.
    , _mdOpenings        :: !OpeningsMap
      -- | Decrypted shares to be used in the third phase.
    , _mdShares          :: !SharesMap
      -- | Vss certificates are added at any time if they are valid and
      -- received from stakeholders.
    , _mdVssCertificates :: !VssCertificatesMap
    } deriving (Show, Generic)

deriveSafeCopySimple 0 'base ''DSPayload
makeLenses ''DSPayload

instance Binary DSPayload
instance MessagePack DSPayload

instance Buildable DSPayload where
    build DSPayload {..} =
        mconcat
            [ formatCommitments
            , formatOpenings
            , formatShares
            , formatCertificates
            ]
      where
        formatIfNotNull formatter l
            | null l = mempty
            | otherwise = bprint formatter l
        formatCommitments =
            formatIfNotNull
                ("  commitments from: "%listJson%"\n")
                (HM.keys _mdCommitments)
        formatOpenings =
            formatIfNotNull
                ("  openings from: "%listJson%"\n")
                (HM.keys _mdOpenings)
        formatShares =
            formatIfNotNull
                ("  shares from: "%listJson%"\n")
                (HM.keys _mdShares)
        formatCertificates =
            formatIfNotNull
                ("  certificates from: "%listJson%"\n")
                (HM.keys _mdVssCertificates)

-- | Verify payload using header containing this payload.
-- TODO: add more checks here.
verifyDSPayload
    :: (SscPayload ssc ~ DSPayload)
    => MainBlockHeader ssc -> SscPayload ssc -> VerificationRes
verifyDSPayload header DSPayload {..} =
    verifyGeneric
        [ ( null _mdCommitments || isCommitmentId slotId
          , "there are commitments in inappropriate block")
        , ( null _mdOpenings || isOpeningId slotId
          , "there are openings in inappropriate block")
        , ( null _mdShares || isSharesId slotId
          , "there are shares in inappropriate block")
        ]
  where
    slotId = header ^. headerSlot

-- | Remove messages irrelevant to given slot id from payload.
filterDSPayload :: SlotId -> DSPayload -> DSPayload
filterDSPayload slotId DSPayload {..} =
    DSPayload
    { _mdCommitments = filteredCommitments
    , _mdOpenings = filteredOpenings
    , _mdShares = filteredShares
    , ..
    }
  where
    filteredCommitments = filterDo isCommitmentId _mdCommitments
    filteredOpenings = filterDo isOpeningId _mdOpenings
    filteredShares = filterDo isSharesId _mdShares
    filterDo
        :: Monoid container
        => (SlotId -> Bool) -> container -> container
    filterDo checker container
        | checker slotId = container
        | otherwise = mempty

----------------------------------------------------------------------------
-- SscProof
----------------------------------------------------------------------------

-- | Proof of MpcData.
-- We can use ADS for commitments, opennings, shares as well,
-- if we find it necessary.
data DSProof = DSProof
    { mpCommitmentsHash     :: !(Hash CommitmentsMap)
    , mpOpeningsHash        :: !(Hash OpeningsMap)
    , mpSharesHash          :: !(Hash SharesMap)
    , mpVssCertificatesHash :: !(Hash VssCertificatesMap)
    } deriving (Show, Eq, Generic)

deriveSafeCopySimple 0 'base ''DSProof

instance Binary DSProof
instance MessagePack DSProof

mkDSProof :: DSPayload -> DSProof
mkDSProof DSPayload {..} =
    DSProof
    { mpCommitmentsHash = hash _mdCommitments
    , mpOpeningsHash = hash _mdOpenings
    , mpSharesHash = hash _mdShares
    , mpVssCertificatesHash = hash _mdVssCertificates
    }

----------------------------------------------------------------------------
-- Utility functions
----------------------------------------------------------------------------

hasCommitment :: PublicKey -> DSPayload -> Bool
hasCommitment pk md = HM.member pk (_mdCommitments md)

hasOpening :: PublicKey -> DSPayload -> Bool
hasOpening pk md = HM.member pk (_mdOpenings md)

hasShares :: PublicKey -> DSPayload -> Bool
hasShares pk md = HM.member pk (_mdShares md)
