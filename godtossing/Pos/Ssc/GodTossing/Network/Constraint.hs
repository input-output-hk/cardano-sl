{-# LANGUAGE DataKinds #-}

module Pos.Ssc.GodTossing.Network.Constraint
       ( GtMessageConstraints
       ) where

import           Universum

import           Data.Tagged                      (Tagged)
import           Node.Message.Class               (Message)

import           Pos.Communication.Limits.Types   (MessageLimited)
import           Pos.Communication.Types.Relay    (DataMsg, InvOrData, ReqMsg, ReqOrRes)
import           Pos.Core                         (HasConfiguration, StakeholderId)
import           Pos.Infra.Configuration          (HasInfraConfiguration)
import           Pos.Ssc.GodTossing.Configuration (HasGtConfiguration)
import           Pos.Ssc.GodTossing.Types.Message (MCCommitment, MCOpening, MCShares,
                                                   MCVssCertificate)

-- TODO: someone who knows networking should take a look because this really
-- doesn't look like something that anyone should ever have to write
type GtMessageConstraints =
    ( Each '[MessageLimited]
        [ DataMsg MCCommitment
        , DataMsg MCOpening
        , DataMsg MCShares
        , DataMsg MCVssCertificate ]
    , Each '[Message]
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
    , HasConfiguration
    , HasGtConfiguration
    , HasInfraConfiguration
    )
