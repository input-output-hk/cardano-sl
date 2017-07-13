{-# LANGUAGE TemplateHaskell #-}

-- | Messages used for communication in GodTossing SSC.

module Pos.Ssc.GodTossing.Types.Message
       ( MCCommitment (..)
       , MCOpening (..)
       , MCShares (..)
       , MCVssCertificate (..)
       , _MCCommitment
       , _MCOpening
       , _MCShares
       , _MCVssCertificate
       , HasGtTag (..)
       , GtTag (..)
       ) where

import           Control.Lens                  (makePrisms)
import qualified Data.Text.Buildable           as Buildable
import           Formatting                    (bprint, build, (%))
import           Universum

import           Pos.Core                      (StakeholderId, addressHash)
import           Pos.Ssc.GodTossing.Core       (InnerSharesMap, Opening, SignedCommitment,
                                                VssCertificate, getCertId)
import           Pos.Ssc.GodTossing.Toss.Types (GtTag (..))

class HasGtTag a where
    toGtTag :: a -> GtTag

data MCCommitment = MCCommitment !SignedCommitment
    deriving (Show, Eq, Generic)

data MCOpening = MCOpening !StakeholderId !Opening
    deriving (Show, Eq, Generic)

data MCShares = MCShares !StakeholderId !InnerSharesMap
    deriving (Show, Eq, Generic)

data MCVssCertificate = MCVssCertificate !VssCertificate
    deriving (Show, Eq, Generic)

makePrisms ''MCCommitment
makePrisms ''MCOpening
makePrisms ''MCShares
makePrisms ''MCVssCertificate

instance Buildable MCCommitment where
    build (MCCommitment (pk, _, _))  =
        bprint ("commitment contents from "%build) $ addressHash pk

instance Buildable MCOpening where
    build (MCOpening k _) =
        bprint ("opening contents from "%build) k

instance Buildable MCShares where
    build (MCShares k _) =
        bprint ("shares contents from "%build) k

instance Buildable MCVssCertificate where
    build (MCVssCertificate c) =
        bprint ("VSS certificate contents from "%build) $ getCertId c

instance HasGtTag MCCommitment where
    toGtTag _ = CommitmentMsg

instance HasGtTag MCOpening where
    toGtTag _ = OpeningMsg

instance HasGtTag MCShares where
    toGtTag _ = SharesMsg

instance HasGtTag MCVssCertificate where
    toGtTag _ = VssCertificateMsg
