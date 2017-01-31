{-# LANGUAGE TemplateHaskell #-}

-- | Types related to Toss.

module Pos.Ssc.GodTossing.Toss.Types
       ( TossModifier (..)
       , tmCommitments
       , tmOpenings
       , tmShares
       , tmCertificates
       ) where

-- import           Universum
import           Control.Lens            (makeLenses)

import           Pos.Ssc.GodTossing.Core (CommitmentsMap, OpeningsMap, SharesMap,
                                          VssCertificatesMap)

data TossModifier = TossModifier
    { _tmCommitments  :: !CommitmentsMap
    , _tmOpenings     :: !OpeningsMap
    , _tmShares       :: !SharesMap
    , _tmCertificates :: !VssCertificatesMap
    }

makeLenses ''TossModifier
