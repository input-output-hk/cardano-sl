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
       , DSMessage(..)
       , DSStorage(..)
       , DSStorageVersion(..)

       -- * Lenses
       -- ** DSPayload
       , mdCommitments
       , mdOpenings
       , mdShares
       , mdVssCertificates
       -- ** DSStorage
       , dsVersionedL
       , dsCurrentSecretL
       , dsLastProcessedSlotL
       -- ** DSStorageVersion
       , dsLocalCommitments
       , dsGlobalCommitments
       , dsLocalShares
       , dsGlobalShares
       , dsLocalOpenings
       , dsGlobalOpenings
       , dsLocalCertificates
       , dsGlobalCertificates

       -- * Utilities
       , hasCommitment
       , hasOpening
       , hasShares

       -- * Instances
       -- ** instance SscTypes SscDynamicState
       ) where

import           Control.Lens         (makeLenses, makeLensesFor)
import           Data.Binary          (Binary)
import           Data.Default         (Default (..))
import qualified Data.HashMap.Strict  as HM
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.MessagePack     (MessagePack)
import           Data.SafeCopy        (base, deriveSafeCopySimple)
import           Data.Tagged          (Tagged (..))
import           Data.Text.Buildable  (Buildable (..))
import           Formatting           (bprint, (%))
import           Serokell.Util        (listJson)
import           Universum

import           Pos.Crypto           (Hash, PublicKey, Share, hash)
import           Pos.FollowTheSatoshi (FtsError)
import           Pos.Genesis          (genesisCertificates)
import           Pos.Ssc.Class.Types  (SscTypes (..))
import           Pos.Types.Slotting   (unflattenSlotId)
import           Pos.Types.Types      (Commitment, CommitmentsMap, Opening, OpeningsMap,
                                       SharesMap, SignedCommitment, SlotId,
                                       VssCertificate, VssCertificatesMap)

----------------------------------------------------------------------------
-- SscMessage
----------------------------------------------------------------------------

data DSMessage
    = DSCommitment !PublicKey
                   !Commitment
    | DSOpening !PublicKey
                !Opening
    | DSShares !PublicKey
               (HashMap PublicKey Share)
    | DSVssCertificate !PublicKey
                       !VssCertificate
    deriving (Show)

----------------------------------------------------------------------------
-- SscStorage
----------------------------------------------------------------------------

data DSStorageVersion = DSStorageVersion
    { -- | Local set of 'Commitment's. These are valid commitments which are
      -- known to the node and not stored in blockchain. It is useful only
      -- for the first 'k' slots, after that it should be discarded.
      _dsLocalCommitments   :: !CommitmentsMap
    , -- | Set of 'Commitment's stored in blocks for current epoch. This can
      -- be calculated by 'mconcat'ing stored commitments, but it would be
      -- inefficient to do it every time we need to know if commitments is
      -- stored in blocks.
      _dsGlobalCommitments  :: !CommitmentsMap
    , -- | Local set of decrypted shares (encrypted shares are stored in
      -- commitments).
      _dsLocalShares        :: !SharesMap
    , -- | Decrypted shares stored in blocks. These shares are guaranteed to
      -- match encrypted shares stored in 'dsGlobalCommitments'.
      _dsGlobalShares       :: !SharesMap
    , -- | Local set of openings
      _dsLocalOpenings      :: !OpeningsMap
    , -- | Openings stored in blocks
      _dsGlobalOpenings     :: !OpeningsMap
    , -- | Local set of VSS certificates
      _dsLocalCertificates  :: !VssCertificatesMap
    , -- | VSS certificates stored in blocks (for all time, not just for
      -- current epoch)
      _dsGlobalCertificates :: !VssCertificatesMap }
      deriving Show

makeLenses ''DSStorageVersion
deriveSafeCopySimple 0 'base ''DSStorageVersion

instance Default DSStorageVersion where
    def =
        DSStorageVersion
        { _dsLocalCommitments = mempty
        , _dsGlobalCommitments = mempty
        , _dsLocalShares = mempty
        , _dsGlobalShares = mempty
        , _dsLocalOpenings = mempty
        , _dsGlobalOpenings = mempty
        , _dsLocalCertificates = mempty
        , _dsGlobalCertificates = genesisCertificates
        }

data DSStorage = DSStorage
    { -- | Last several versions of MPC storage, a version for each received
      -- block. To bring storage to the state as it was just before the last
      -- block arrived, just remove the head. All incoming commitments/etc
      -- which aren't parts of blocks are applied to the head, too.
      --
      -- TODO: this is a very naive solution. A better one would be storing
      -- deltas for maps which are in 'DSStorageVersion', see [POS-25] for
      -- the explanation of deltas.
      _dsVersioned         :: NonEmpty DSStorageVersion
    , -- | Secret that we are using for the current epoch.
      _dsCurrentSecret     :: !(Maybe (PublicKey, SignedCommitment, Opening))
    , -- | Last slot we are aware of.
      _dsLastProcessedSlot :: !SlotId
    }

flip makeLensesFor ''DSStorage
    [ ("_dsVersioned", "dsVersionedL")
    , ("_dsCurrentSecret", "dsCurrentSecretL")
    , ("_dsLastProcessedSlot", "dsLastProcessedSlotL")
    ]
deriveSafeCopySimple 0 'base ''DSStorage

instance Default DSStorage where
    def =
        DSStorage
        { _dsVersioned = (def :| [])
        , _dsCurrentSecret = Nothing
        , _dsLastProcessedSlot = unflattenSlotId 0
        }

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

----------------------------------------------------------------------------
-- Utility functions
----------------------------------------------------------------------------

hasCommitment :: PublicKey -> DSPayload -> Bool
hasCommitment pk md = HM.member pk (_mdCommitments md)

hasOpening :: PublicKey -> DSPayload -> Bool
hasOpening pk md = HM.member pk (_mdOpenings md)

hasShares :: PublicKey -> DSPayload -> Bool
hasShares pk md = HM.member pk (_mdShares md)

----------------------------------------------------------------------------
-- 'SscDynamicState' and its instances
----------------------------------------------------------------------------

data SscDynamicState

instance SscTypes SscDynamicState where
    type SscStorage   SscDynamicState = DSStorage
    type SscPayload   SscDynamicState = DSPayload
    type SscProof     SscDynamicState = DSProof
    type SscMessage   SscDynamicState = DSMessage
    type SscSeedError SscDynamicState = FtsError

    mkSscPayload = notImplemented
    mkSscProof = Tagged $
        \DSPayload {..} -> DSProof
             { mpCommitmentsHash = hash _mdCommitments
             , mpOpeningsHash = hash _mdOpenings
             , mpSharesHash = hash _mdShares
             , mpVssCertificatesHash = hash _mdVssCertificates
             }
