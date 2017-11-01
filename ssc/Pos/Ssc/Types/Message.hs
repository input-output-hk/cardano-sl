-- | Messages used for communication in GodTossing SSC.

module Pos.Ssc.Types.Message
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

import           Control.Lens                  (makePrisms)
import qualified Data.Text.Buildable           as Buildable
import           Formatting                    (bprint, build, (%))
import           Universum

import           Pos.Core                      (StakeholderId, VssCertificate,
                                                addressHash, getCertId)
import           Pos.Ssc.Core                  (InnerSharesMap, Opening, SignedCommitment)
import           Pos.Ssc.Toss.Types            (SscTag (..))

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
