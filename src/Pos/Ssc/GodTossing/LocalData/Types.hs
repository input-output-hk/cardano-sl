{-# LANGUAGE CPP             #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Pos.Ssc.GodTossing.LocalData.Types
       ( GtLocalData (..)
       , ldCommitments
       , ldOpenings
       , ldShares
       , ldCertificates
       , ldLastProcessedSlot
       ) where

import           Control.Lens                   (makeLenses)
-- import           Universum

import           Pos.Ssc.GodTossing.Core        (CommitmentsMap, OpeningsMap, SharesMap)
import qualified Pos.Ssc.GodTossing.VssCertData as VCD (VssCertData)
import           Pos.Types                      (SlotId)

data GtLocalData = GtLocalData
    { -- | Local set of 'Commitment's. These are valid commitments which are
      -- known to the node and not stored in blockchain. It is useful only
      -- for the first 'k' slots, after that it should be discarded.
      _ldCommitments       :: !CommitmentsMap
    , -- | Local set of openings
      _ldOpenings          :: !OpeningsMap
    , -- | Local set of decrypted shares (encrypted shares are stored in
      -- commitments).
      _ldShares            :: !SharesMap
    , -- | Local set of VSS certificates
      _ldCertificates      :: !VCD.VssCertData
    , -- | Last slot we are aware of.
      _ldLastProcessedSlot :: !SlotId
    }

makeLenses ''GtLocalData
