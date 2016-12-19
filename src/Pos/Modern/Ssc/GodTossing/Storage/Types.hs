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
       ( GtGlobalStateM (..)
       -- * Lenses
       -- ** GtGlobalState
       , gsCommitments
       , gsOpenings
       , gsShares
       , gsVssCertificates
       ) where

import           Control.Lens                   (makeLenses)
import           Data.Default                   (Default (..))
import           Data.SafeCopy                  (base, deriveSafeCopySimple)
import           Universum

import           Pos.Ssc.GodTossing.Types.Base  (CommitmentsMap, OpeningsMap, SharesMap,
                                                 VssCertificatesMap)

-- | MPC-related content of main body.
data GtGlobalStateM = GtGlobalStateM
    { -- | Commitments are added during the first phase of epoch.
      _gsCommitments     :: !CommitmentsMap
      -- | Openings are added during the second phase of epoch.
    , _gsOpenings        :: !OpeningsMap
      -- | Decrypted shares to be used in the third phase.
    , _gsShares          :: !SharesMap
      -- | Vss certificates are added at any time if they are valid and
      -- received from stakeholders.
    , _gsVssCertificates :: !VssCertificatesMap
    } deriving (Show, Generic)

deriveSafeCopySimple 0 'base ''GtGlobalStateM
makeLenses ''GtGlobalStateM

instance Default GtGlobalStateM where
    def =
        GtGlobalStateM
        {
          _gsCommitments = mempty
        , _gsOpenings = mempty
        , _gsShares = mempty
        , _gsVssCertificates = mempty
        }
