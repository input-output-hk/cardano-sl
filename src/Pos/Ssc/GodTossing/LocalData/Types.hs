{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Pos.Ssc.GodTossing.LocalData.Types
       ( gtLocalCommitments
       , gtLocalOpenings
       , gtLocalShares
       , gtLocalCertificates
       , GtLocalData (..)
       ) where

import           Control.Lens                  (makeLenses)
import           Data.Default                  (Default (def))
import           Universum

import           Pos.Ssc.GodTossing.Types.Base (CommitmentsMap, OpeningsMap, SharesMap,
                                                VssCertificatesMap)

data GtLocalData = GtLocalData
    { -- | Local set of 'Commitment's. These are valid commitments which are
      -- known to the node and not stored in blockchain. It is useful only
      -- for the first 'k' slots, after that it should be discarded.
      _gtLocalCommitments  :: !CommitmentsMap
    , -- | Local set of openings
      _gtLocalOpenings     :: !OpeningsMap
    , -- | Local set of decrypted shares (encrypted shares are stored in
      -- commitments).
      _gtLocalShares       :: !SharesMap
    , -- | Local set of VSS certificates
      _gtLocalCertificates :: !VssCertificatesMap
    }

makeLenses ''GtLocalData

instance Default GtLocalData where
    def =
        GtLocalData
        { _gtLocalCertificates = mempty
        , _gtLocalShares = mempty
        , _gtLocalOpenings = mempty
        , _gtLocalCommitments = mempty
        }
