{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Some types related to GodTossing necessary for Ssc instance.

module Pos.Ssc.GodTossing.Types.Types
       (
         -- * Instance types
         GtPayload (..)
       , GtProof (..)
       , GtGlobalState (..)
       , GtContext (..)
       , GtParams (..)

       -- * Lenses
       -- ** GtPayload
       , gsCommitments
       , gsOpenings
       , gsShares
       , gsVssCertificates
       , mkGtProof
       , createGtContext
       , _gpCertificates
       , emptyPayload

       , SscBi
       ) where

import           Control.Concurrent.STM        (newTVarIO)
import qualified Control.Concurrent.STM        as STM
import           Control.Lens                  (makeLenses)
import           Data.Default                  (Default, def)
import qualified Data.HashMap.Strict           as HM
import           Data.SafeCopy                 (base, deriveSafeCopySimple)
import qualified Data.Text                     as T
import           Data.Text.Buildable           (Buildable (..))
import           Data.Text.Lazy.Builder        (Builder, fromText)
import           Formatting                    (bprint, sformat, (%))
import           Serokell.Util                 (listJson)
import           Universum

import           Pos.Binary.Class              (Bi)
import           Pos.Crypto                    (Hash, VssKeyPair, hash)
import           Pos.Ssc.GodTossing.Genesis    (genesisCertificates)
import           Pos.Ssc.GodTossing.Types.Base (Commitment, CommitmentsMap, Opening,
                                                OpeningsMap, SharesMap, VssCertificate,
                                                VssCertificatesMap)

----------------------------------------------------------------------------
-- SscGlobalState
----------------------------------------------------------------------------

-- | Global state of GodTossing, contains relevant SSC data from blocks.
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

instance Buildable GtGlobalState where
    build GtGlobalState {..} =
        formatMPC $ mconcat
            [ formatCommitments
            , formatOpenings
            , formatShares
            , formatCertificates
            ]
      where
        formatMPC :: Text -> Builder
        formatMPC msg
            | T.null msg = "  no MPC data"
            | otherwise = fromText msg
        formatIfNotNull formatter l
            | null l = mempty
            | otherwise = sformat formatter l
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

instance Default GtGlobalState where
    def =
        GtGlobalState
        {
          _gsCommitments = mempty
        , _gsOpenings = mempty
        , _gsShares = mempty
        , _gsVssCertificates = mempty
        }

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

emptyPayload :: GtPayload
emptyPayload = CertificatesPayload mempty

instance Default GtPayload where
    def = CertificatesPayload genesisCertificates

_gpCertificates :: GtPayload -> VssCertificatesMap
_gpCertificates (CommitmentsPayload _ certs) = certs
_gpCertificates (OpeningsPayload _ certs)    = certs
_gpCertificates (SharesPayload _ certs)      = certs
_gpCertificates (CertificatesPayload certs)  = certs

deriveSafeCopySimple 0 'base ''GtPayload

isEmptyGtPayload :: GtPayload -> Bool
isEmptyGtPayload (CommitmentsPayload comms certs) = null comms && null certs
isEmptyGtPayload (OpeningsPayload opens certs)    = null opens && null certs
isEmptyGtPayload (SharesPayload shares certs)     = null shares && null certs
isEmptyGtPayload (CertificatesPayload certs)      = null certs

instance Buildable GtPayload where
    build gp
        | isEmptyGtPayload gp = "  no GodTossing payload"
        | otherwise =
            case gp of
                CommitmentsPayload comms certs ->
                    formatTwo formatCommitments comms certs
                OpeningsPayload openings certs ->
                    formatTwo formatOpenings openings certs
                SharesPayload shares certs ->
                    formatTwo formatShares shares certs
                CertificatesPayload certs -> formatCertificates certs
      where
        formatIfNotNull formatter l
            | null l = mempty
            | otherwise = bprint formatter l
        formatCommitments comms =
            formatIfNotNull
                ("  commitments from: " %listJson % "\n")
                (HM.keys comms)
        formatOpenings openings =
            formatIfNotNull
                ("  openings from: " %listJson % "\n")
                (HM.keys openings)
        formatShares shares =
            formatIfNotNull
                ("  shares from: " %listJson % "\n")
                (HM.keys shares)
        formatCertificates certs =
            formatIfNotNull
                ("  certificates from: " %listJson % "\n")
                (HM.keys certs)
        formatTwo formatter hm certs =
            mconcat [formatter hm, formatCertificates certs]

----------------------------------------------------------------------------
-- SscProof
----------------------------------------------------------------------------

-- | Proof of MpcData.
-- We can use ADS for commitments, openings, shares as well,
-- if we find it necessary.
data GtProof
    = CommitmentsProof !(Hash CommitmentsMap) !(Hash VssCertificatesMap)
    | OpeningsProof !(Hash OpeningsMap) !(Hash VssCertificatesMap)
    | SharesProof !(Hash SharesMap) !(Hash VssCertificatesMap)
    | CertificatesProof !(Hash VssCertificatesMap)
    deriving (Show, Eq, Generic)

deriveSafeCopySimple 0 'base ''GtProof

-- | Smart constructor for 'GtProof' from 'GtPayload'.
mkGtProof
    :: (Bi VssCertificate, Bi Commitment, Bi Opening)
    => GtPayload -> GtProof
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

data GtParams = GtParams
    {
      gtpRebuildDb  :: !Bool
    , gtpSscEnabled :: !Bool              -- ^ Whether node should participate in SSC
                                          -- in case SSC requires participation.
    , gtpVssKeyPair :: !VssKeyPair        -- ^ Key pair used for secret sharing
    }

data GtContext = GtContext
    {
      -- | Vss key pair used for MPC.
      gtcVssKeyPair             :: !VssKeyPair
    , gtcParticipateSsc         :: !(STM.TVar Bool)
    , gtcVssCertificateVerified :: !(STM.TVar Bool)
    }

createGtContext :: MonadIO m => GtParams -> m GtContext
createGtContext GtParams {..} =
    GtContext gtpVssKeyPair
           <$> liftIO (newTVarIO gtpSscEnabled)
           <*> liftIO (newTVarIO False)

----------------------------------------------------------------------------
-- Convinient binary type alias
----------------------------------------------------------------------------

type SscBi =
    ( Bi GtProof
    , Bi GtPayload
    , Bi Opening
    , Bi VssCertificate
    , Bi Commitment)
