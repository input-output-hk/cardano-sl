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
import           Universum

import           Pos.Ssc.GodTossing.Core       (InnerSharesMap, Opening, SignedCommitment,
                                                VssCertificate)
import           Pos.Ssc.GodTossing.Toss.Types (GtTag (..))
import           Pos.Types                     (StakeholderId)

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
    build _ = "commitment contents"

instance Buildable MCOpening where
    build _ = "opening contents"

instance Buildable MCShares where
    build _ = "shares contents"

instance Buildable MCVssCertificate where
    build _ = "VSS certificate contents"


instance HasGtTag MCCommitment where
    toGtTag _ = CommitmentMsg

instance HasGtTag MCOpening where
    toGtTag _ = OpeningMsg

instance HasGtTag MCShares where
    toGtTag _ = SharesMsg

instance HasGtTag MCVssCertificate where
    toGtTag _ = VssCertificateMsg
