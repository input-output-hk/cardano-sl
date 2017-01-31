{-# LANGUAGE ViewPatterns #-}

-- | Messages used for communication in GodTossing SSC.

module Pos.Ssc.GodTossing.Types.Message
       ( GtMsgTag (..)
       , GtMsgContents (..)
       , msgContentsTag
       ) where

import qualified Data.Text.Buildable           as Buildable
import           Universum

import           Pos.Ssc.GodTossing.Core       (InnerSharesMap, Opening, SignedCommitment,
                                                VssCertificate)
import           Pos.Ssc.GodTossing.Toss.Types (GtMsgTag (..))
import           Pos.Types                     (StakeholderId)

-- | Data message. Can be used to send actual data.
data GtMsgContents
    = MCCommitment !SignedCommitment
    | MCOpening !StakeholderId !Opening
    | MCShares !StakeholderId !InnerSharesMap
    | MCVssCertificate !VssCertificate
    deriving (Show, Eq, Generic)

instance Buildable GtMsgContents where
    build (msgContentsTag -> tag) = Buildable.build tag <> " contents"

-- | GtMsgTag appropriate for given DataMsg.
msgContentsTag :: GtMsgContents -> GtMsgTag
msgContentsTag (MCCommitment _)     = CommitmentMsg
msgContentsTag (MCOpening _ _)      = OpeningMsg
msgContentsTag (MCShares _ _)       = SharesMsg
msgContentsTag (MCVssCertificate _) = VssCertificateMsg
