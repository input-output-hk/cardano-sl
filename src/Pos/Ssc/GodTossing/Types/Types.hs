{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Some types related to GodTossing necessary for Ssc instance.

module Pos.Ssc.GodTossing.Types.Types
       (
         -- * Instance types
         GtPayload (..)
       , GtProof (..)
       , GtGlobalState (..)

       -- * Lenses
       -- ** GtPayload
       , gsCommitments
       , gsOpenings
       , gsShares
       , gsVssCertificates
       , mkGtProof
       , _gpCertificates
       ) where

import           Control.Lens                  (makeLenses)
import           Data.Binary                   (Binary)
import qualified Data.HashMap.Strict           as HM
import           Data.MessagePack              (MessagePack)
import           Data.SafeCopy                 (base, deriveSafeCopySimple)
import           Data.Text.Buildable           (Buildable (..))
import           Formatting                    (bprint, (%))
import           Serokell.Util                 (listJson)
import           Universum

import           Pos.Crypto                    (Hash, hash)
import           Pos.Ssc.GodTossing.Types.Base (CommitmentsMap, OpeningsMap, SharesMap,
                                                VssCertificatesMap)

----------------------------------------------------------------------------
-- SscGlobalState
----------------------------------------------------------------------------
-- | MPC-related content of main body.
data GtGlobalState = GtGlobalState
    { -- | Commitments are added during the first phase of epoch.
      _gsCommitments     :: !CommitmentsMap
      -- | Openings are added during the second phase of epoch.
    , _gsOpenings        :: !OpeningsMap
      -- | Decrypted shares to be used in the third phase.
    , _gsShares          :: !SharesMap
      -- | Vss certificates are added at any time if they are valid and
      -- received from stakeholders.
    , _gsVssCertificates :: !VssCertificatesMap
    } deriving (Show, Generic)

deriveSafeCopySimple 0 'base ''GtGlobalState
makeLenses ''GtGlobalState

instance Binary GtGlobalState
instance MessagePack GtGlobalState

instance Buildable GtGlobalState where
    build GtGlobalState {..} =
        mconcat
            [
              formatCommitments
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
                (HM.keys _gsCommitments)
        formatOpenings =
            formatIfNotNull
                ("  openings from: "%listJson%"\n")
                (HM.keys _gsOpenings)
        formatShares =
            formatIfNotNull
                ("  shares from: "%listJson%"\n")
                (HM.keys _gsShares)
        formatCertificates =
            formatIfNotNull
                ("  certificates from: "%listJson%"\n")
                (HM.keys _gsVssCertificates)

----------------------------------------------------------------------------
-- SscPayload
----------------------------------------------------------------------------
-- | Block payload
data GtPayload
    = CommitmentsPayload  !CommitmentsMap !VssCertificatesMap
    | OpeningsPayload     !OpeningsMap    !VssCertificatesMap
    | SharesPayload       !SharesMap      !VssCertificatesMap
    | CertificatesPayload !VssCertificatesMap
    deriving (Show, Generic)

_gpCertificates :: GtPayload -> VssCertificatesMap
_gpCertificates (CommitmentsPayload _ certs) = certs
_gpCertificates (OpeningsPayload _ certs) = certs
_gpCertificates (SharesPayload _ certs) = certs
_gpCertificates (CertificatesPayload certs) = certs

deriveSafeCopySimple 0 'base ''GtPayload
makeLenses ''GtPayload

instance Binary GtPayload
instance MessagePack GtPayload

instance Buildable GtPayload where
    build gp =
        case gp of
            CommitmentsPayload comms certs     ->
                formatTwo formatCommitments comms certs
            OpeningsPayload  openings certs    ->
                formatTwo formatOpenings openings certs
            SharesPayload shares certs         ->
                formatTwo formatShares shares certs
            CertificatesPayload certs          ->
                formatCertificates certs
      where
        formatIfNotNull formatter l
            | null l = mempty
            | otherwise = bprint formatter l
        formatCommitments comms =
            formatIfNotNull
                ("  commitments from: "%listJson%"\n")
                (HM.keys comms)
        formatOpenings openings =
            formatIfNotNull
                ("  openings from: "%listJson%"\n")
                (HM.keys openings)
        formatShares shares =
            formatIfNotNull
                ("  shares from: "%listJson%"\n")
                (HM.keys shares)
        formatCertificates certs =
            formatIfNotNull
                ("  certificates from: "%listJson%"\n")
                (HM.keys certs)
        formatTwo formatter hm certs =
              mconcat
                  [
                    formatter hm
                  , formatCertificates certs
                  ]

----------------------------------------------------------------------------
-- SscProof
----------------------------------------------------------------------------
-- | Proof of MpcData.
-- We can use ADS for commitments, opennings, shares as well,
-- if we find it necessary.
data GtProof
    = CommitmentsProof !(Hash CommitmentsMap) !(Hash VssCertificatesMap)
    | OpeningsProof !(Hash OpeningsMap) !(Hash VssCertificatesMap)
    | SharesProof !(Hash SharesMap) !(Hash VssCertificatesMap)
    | CertificatesProof !(Hash VssCertificatesMap)
    deriving (Show, Eq, Generic)

deriveSafeCopySimple 0 'base ''GtProof

instance Binary GtProof
instance MessagePack GtProof

-- | Smart constructor for 'GtProof' from 'GtPayload'.
mkGtProof :: GtPayload -> GtProof
mkGtProof payload =
    case payload of
        CommitmentsPayload comms certs ->
            proof CommitmentsProof comms certs
        OpeningsPayload openings certs ->
            proof OpeningsProof openings certs
        SharesPayload shares certs     ->
            proof SharesProof shares certs
        CertificatesPayload certs      ->
            CertificatesProof $ hash certs
      where
        proof constr hm cert =
            constr (hash hm) (hash cert)
