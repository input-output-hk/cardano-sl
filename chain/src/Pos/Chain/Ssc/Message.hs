{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Messages used for communication in SSC.

module Pos.Chain.Ssc.Message
       ( MCCommitment (..)
       , MCOpening (..)
       , MCShares (..)
       , MCVssCertificate (..)
       , _MCCommitment
       , _MCOpening
       , _MCShares
       , _MCVssCertificate
       , HasSscTag (..)
       , SscTag (..)
       ) where

import           Universum

import           Control.Lens (makePrisms)
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable as Buildable

import           Pos.Chain.Ssc.Commitment (SignedCommitment)
import           Pos.Chain.Ssc.Opening (Opening)
import           Pos.Chain.Ssc.SharesMap (InnerSharesMap)
import           Pos.Chain.Ssc.Toss.Types (SscTag (..))
import           Pos.Chain.Ssc.VssCertificate (VssCertificate, getCertId)
import           Pos.Core (StakeholderId, addressHash)

class HasSscTag a where
    toSscTag :: a -> SscTag

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

instance HasSscTag MCCommitment where
    toSscTag _ = CommitmentMsg

instance HasSscTag MCOpening where
    toSscTag _ = OpeningMsg

instance HasSscTag MCShares where
    toSscTag _ = SharesMsg

instance HasSscTag MCVssCertificate where
    toSscTag _ = VssCertificateMsg
