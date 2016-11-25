{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Some types related to GodTossing necessary for Ssc instance.

module Pos.Ssc.GodTossing.Types.Types
       (
         -- * Instance types
         GtPayload(..)
       , GtProof(..)

       -- * Lenses
       -- ** GtPayload
       , mdCommitments
       , mdOpenings
       , mdShares
       , mdVssCertificates
       , mkGtProof
       ) where

import           Control.Lens                  (makeLenses)
import           Data.Binary                   (Binary)
import qualified Data.HashMap.Strict           as HM
import           Data.MessagePack              (MessagePack)
import           Data.SafeCopy                 (base, deriveSafeCopySimple)
import qualified Data.Text                     as T
import           Data.Text.Buildable           (Buildable (..))
import           Data.Text.Lazy.Builder        (fromText)
import           Formatting                    (sformat, (%))
import           Serokell.Util                 (listJson)
import           Universum

import           Pos.Crypto                    (Hash, hash)
import           Pos.Ssc.GodTossing.Types.Base (CommitmentsMap, OpeningsMap, SharesMap,
                                                VssCertificatesMap)

----------------------------------------------------------------------------
-- SscPayload
----------------------------------------------------------------------------
-- | MPC-related content of main body.
data GtPayload = GtPayload
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

deriveSafeCopySimple 0 'base ''GtPayload
makeLenses ''GtPayload

instance Binary GtPayload
instance MessagePack GtPayload

instance Buildable GtPayload where
    build GtPayload {..} =
        formatMPC $ mconcat
            [ (formatCommitments :: Text)
            , formatOpenings
            , formatShares
            , formatCertificates
            ]
      where
        formatMPC msg
            | T.null msg = "  no MPC data"
            | otherwise = fromText msg
        formatIfNotNull formatter l
            | null l = mempty
            | otherwise = sformat formatter l
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
data GtProof = GtProof
    { mpCommitmentsHash     :: !(Hash CommitmentsMap)
    , mpOpeningsHash        :: !(Hash OpeningsMap)
    , mpSharesHash          :: !(Hash SharesMap)
    , mpVssCertificatesHash :: !(Hash VssCertificatesMap)
    } deriving (Show, Eq, Generic)

deriveSafeCopySimple 0 'base ''GtProof

instance Binary GtProof
instance MessagePack GtProof

-- | Smart constructor for 'GtProof' from 'GtPayload'.
mkGtProof :: GtPayload -> GtProof
mkGtProof GtPayload {..} =
    GtProof
    { mpCommitmentsHash = hash _mdCommitments
    , mpOpeningsHash = hash _mdOpenings
    , mpSharesHash = hash _mdShares
    , mpVssCertificatesHash = hash _mdVssCertificates
    }

