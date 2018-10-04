{-# LANGUAGE DataKinds #-}

-- | Messages used for communication in SSC.

module Pos.Ssc.Message
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
       , SscMessageConstraints
       ) where

import           Universum

import           Control.Lens (makePrisms)
import           Data.Tagged (Tagged)
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable as Buildable
import           Node.Message.Class (Message)

import           Pos.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Pos.Core (StakeholderId, VssCertificate, addressHash,
                     getCertId)
import           Pos.Core.Ssc (InnerSharesMap, Opening, SignedCommitment)
import           Pos.Infra.Communication.Types.Relay (DataMsg (..), InvOrData,
                     ReqMsg, ReqOrRes)
import           Pos.Ssc.Toss.Types (SscTag (..))

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

instance Bi (DataMsg MCCommitment) where
    encode (DataMsg (MCCommitment signedComm)) = encode signedComm
    decode = DataMsg . MCCommitment <$> decode

instance Bi (DataMsg MCOpening) where
    encode (DataMsg (MCOpening sId opening)) = encodeListLen 2 <> encode sId <> encode opening
    decode = do
        enforceSize "DataMsg MCOpening" 2
        DataMsg <$> (MCOpening <$> decode <*> decode)

instance Bi (DataMsg MCShares) where
    encode (DataMsg (MCShares sId innerMap)) = encodeListLen 2 <> encode sId <> encode innerMap
    decode = do
        enforceSize "DataMsg MCShares" 2
        DataMsg <$> (MCShares <$> decode <*> decode)

instance Bi (DataMsg MCVssCertificate) where
    encode (DataMsg (MCVssCertificate vss)) = encode vss
    decode = DataMsg . MCVssCertificate <$> decode


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

-- TODO: someone who knows networking should take a look because this really
-- doesn't look like something that anyone should ever have to write
type SscMessageConstraints =
    ( Each '[Message]
        [ InvOrData (Tagged MCCommitment     StakeholderId) MCCommitment
        , InvOrData (Tagged MCOpening        StakeholderId) MCOpening
        , InvOrData (Tagged MCShares         StakeholderId) MCShares
        , InvOrData (Tagged MCVssCertificate StakeholderId) MCVssCertificate ]
    , Each '[Message]
        [ ReqMsg (Tagged MCCommitment     StakeholderId)
        , ReqMsg (Tagged MCOpening        StakeholderId)
        , ReqMsg (Tagged MCShares         StakeholderId)
        , ReqMsg (Tagged MCVssCertificate StakeholderId) ]
    , Each '[Message]
        [ ReqOrRes (Tagged MCCommitment     StakeholderId)
        , ReqOrRes (Tagged MCOpening        StakeholderId)
        , ReqOrRes (Tagged MCShares         StakeholderId)
        , ReqOrRes (Tagged MCVssCertificate StakeholderId) ]
    )
