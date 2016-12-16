{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilies           #-}

-- | GodTossing global state.

module Pos.Modern.Ssc.GodTossing.Storage.Types
       ( GtGlobalState (..)
       -- * Lenses
       -- ** GtGlobalState
       , gsCommitments
       , gsOpenings
       , gsShares
       , gsVssCertificates
       , gsBlocksMpc
       ) where

import           Control.Lens                   (makeLenses)
import           Data.Default                   (Default (..))
import           Data.List.NonEmpty             (NonEmpty ((:|)))
import           Data.SafeCopy                  (base, deriveSafeCopySimple)
import           Universum

import           Pos.Ssc.GodTossing.Types.Base  (CommitmentsMap, OpeningsMap, SharesMap,
                                                 VssCertificatesMap)
import           Pos.Ssc.GodTossing.Types.Types (GtPayload)

-- | MPC-related content of main body.
data GtGlobalState = GtGlobalState
    { -- | Commitments are added during the first phase of epoch.
      _gsCommitments     :: !CommitmentsMap
      -- | Openings are added during the second phase of epoch.
    , _gsOpenings        :: !OpeningsMap
      -- | Decrypted shares to be used in the third phase.
    , _gsShares          :: !SharesMap
      -- | Vss certificates are added at any time if they are valid and
      -- received from stakeholders.
    , _gsVssCertificates :: !VssCertificatesMap
      -- | MPC data from last several blocks.
      -- Head - last arrived block.
      -- _gsCommitments, _gsOpenings, _gsShares and _gsVssCertificates are
      -- unions by all blocks in this list.
    , _gsBlocksMpc       :: NonEmpty GtPayload
    } deriving (Show, Generic)

deriveSafeCopySimple 0 'base ''GtGlobalState
makeLenses ''GtGlobalState

instance Default GtGlobalState where
    def =
        GtGlobalState
        {
          _gsCommitments = mempty
        , _gsOpenings = mempty
        , _gsShares = mempty
        , _gsVssCertificates = mempty
        , _gsBlocksMpc = def :| []
        }
