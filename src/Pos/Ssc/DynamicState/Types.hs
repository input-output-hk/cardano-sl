{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | A “dynamic state” implementation of SSC. Nodes exchange commitments,
-- openings, and shares, and in the end arrive at a shared seed.
--
-- See https://eprint.iacr.org/2015/889.pdf (“A Provably Secure
-- Proof-of-Stake Blockchain Protocol”), section 4 for more details.

module Pos.Ssc.DynamicState.Types
       ( SscDynamicState
       , DSPayload(..)
       , DSProof(..)

       -- * Instances
       -- ** instance SscTypes SscDynamicState
       ) where

import           Data.Binary          (Binary)
import qualified Data.HashMap.Strict  as HM
import           Data.MessagePack     (MessagePack)
import           Data.SafeCopy        (base, deriveSafeCopySimple)
import           Data.Tagged          (Tagged (..))
import           Data.Text.Buildable  (Buildable (..))
import           Formatting           (bprint, (%))
import           Serokell.Util        (listJson)
import           Universum

import           Pos.Crypto           (Hash, hash)
import           Pos.FollowTheSatoshi (FtsError)
import           Pos.Ssc.Class.Types  (SscTypes (..))
import           Pos.Types.Types      (CommitmentsMap, FtsSeed, OpeningsMap, SharesMap,
                                       SlotId, VssCertificatesMap)

data SscDynamicState

instance SscTypes SscDynamicState where
    -- type SscInternalState SscDynamicState = DSState
    type SscInternalState SscDynamicState = NotImplemented
    type SscPayload       SscDynamicState = DSPayload
    type SscProof         SscDynamicState = DSProof
    -- type SscMessage       SscDynamicState = DSMessage
    type SscMessage       SscDynamicState = NotImplemented
    type SscSeedError     SscDynamicState = FtsError
    mkSscPayload = notImplemented
    mkSscProof = Tagged $
        \DSPayload {..} -> DSProof
             { mpCommitmentsHash = hash _mdCommitments
             , mpOpeningsHash = hash _mdOpenings
             , mpSharesHash = hash _mdShares
             , mpVssCertificatesHash = hash _mdVssCertificates
             }

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
    } deriving (Generic)

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

-- | Proof of MpcData.
-- We can use ADS for commitments, opennings, shares as well,
-- if we find it necessary.
data DSProof = DSProof
    { mpCommitmentsHash     :: !(Hash CommitmentsMap)
    , mpOpeningsHash        :: !(Hash OpeningsMap)
    , mpSharesHash          :: !(Hash SharesMap)
    , mpVssCertificatesHash :: !(Hash VssCertificatesMap)
    } deriving (Show, Eq, Generic)

instance Binary DSProof
instance MessagePack DSProof

----------------------------------------------------------------------------
-- TH-derived instances
----------------------------------------------------------------------------

deriveSafeCopySimple 0 'base ''DSPayload
deriveSafeCopySimple 0 'base ''DSProof
