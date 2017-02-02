{-# LANGUAGE ViewPatterns #-}

-- | Messages used for communication in GodTossing SSC.

module Pos.Ssc.GodTossing.Types.Message
       ( GtTag (..)
       , GtMsgContents (..)
       , msgContentsTag
       ) where

import qualified Data.Text.Buildable           as Buildable
import           Universum

import           Pos.Ssc.GodTossing.Core       (InnerSharesMap, MultiCommitment,
                                                MultiOpening, VssCertificate)
import           Pos.Ssc.GodTossing.Toss.Types (GtTag (..))
import           Pos.Types                     (StakeholderId)

-- | Data message. Can be used to send actual data.
data GtMsgContents
    = MCCommitment !MultiCommitment
    | MCOpening !StakeholderId !MultiOpening
    | MCShares !StakeholderId !InnerSharesMap
    | MCVssCertificate !VssCertificate
    deriving (Show, Eq, Generic)

instance Buildable GtMsgContents where
    build (msgContentsTag -> tag) = Buildable.build tag <> " contents"

-- | GtTag appropriate for given DataMsg.
msgContentsTag :: GtMsgContents -> GtTag
msgContentsTag (MCCommitment _)     = CommitmentMsg
msgContentsTag (MCOpening _ _)      = OpeningMsg
msgContentsTag (MCShares _ _)       = SharesMsg
msgContentsTag (MCVssCertificate _) = VssCertificateMsg
