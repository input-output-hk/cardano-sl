{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

-- | Messages used for communication in GodTossing SSC.

module Pos.Ssc.GodTossing.Types.Message
       ( GtTag (..)
       , GtMsgContents (..)
       , msgContentsTag

       , _MCCommitment
       , _MCOpening
       , _MCShares
       , _MCVssCertificate
       ) where

import           Control.Lens                  (makePrisms)
import qualified Data.Text.Buildable           as Buildable
import           Universum

import           Pos.Ssc.GodTossing.Core       (InnerSharesMap, Opening, SignedCommitment,
                                                VssCertificate)
import           Pos.Ssc.GodTossing.Toss.Types (GtTag (..))
import           Pos.Types                     (StakeholderId)

-- | Data message. Can be used to send actual data.
data GtMsgContents
    = MCCommitment !SignedCommitment
    | MCOpening !StakeholderId !Opening
    | MCShares !StakeholderId !InnerSharesMap
    | MCVssCertificate !VssCertificate
    deriving (Show, Eq, Generic)

makePrisms ''GtMsgContents

instance Buildable GtMsgContents where
    build (msgContentsTag -> tag) = Buildable.build tag <> " contents"

-- | GtTag appropriate for given DataMsg.
msgContentsTag :: GtMsgContents -> GtTag
msgContentsTag (MCCommitment _)     = CommitmentMsg
msgContentsTag (MCOpening _ _)      = OpeningMsg
msgContentsTag (MCShares _ _)       = SharesMsg
msgContentsTag (MCVssCertificate _) = VssCertificateMsg
