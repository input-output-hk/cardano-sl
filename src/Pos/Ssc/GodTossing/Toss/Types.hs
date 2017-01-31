{-# LANGUAGE TemplateHaskell #-}

-- | Types related to Toss.

module Pos.Ssc.GodTossing.Toss.Types
       ( TossModifier (..)
       , tmCommitments
       , tmOpenings
       , tmShares
       , tmCertificates
       ) where

import           Control.Lens            (makeLenses)
import           Universum

import           Pos.Ssc.GodTossing.Core (CommitmentsMap, OpeningsMap, SharesMap,
                                          VssCertificatesMap)

data TossModifier = TossModifier
    { _tmCommitments  :: !CommitmentsMap
    , _tmOpenings     :: !OpeningsMap
    , _tmShares       :: !SharesMap
    , _tmCertificates :: !VssCertificatesMap
    }

makeLenses ''TossModifier

instance Monoid TossModifier where
    mempty = TossModifier mempty mempty mempty mempty
    mappend (TossModifier leftComms leftOpens leftShares leftCerts)
            (TossModifier rightComms rightOpens rightShares rightCerts) =
        TossModifier
        { _tmCommitments = rightComms <> leftComms
        , _tmOpenings = rightOpens <> leftOpens
        , _tmShares = rightShares <> leftShares
        , _tmCertificates = rightCerts <> leftCerts
        }

instance Semigroup TossModifier
