{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Internal state of the MPC algorithm (“multi-party computation”) – the
-- algorithm which computes a shared seed with other nodes and decides which
-- nodes will be leaders of the next epoch.

module Pos.State.Storage.Mpc
       (
         MpcStorage
       , HasMpcStorage(mpcStorage)

       , getLeaders
       ) where

import           Control.Lens  (makeClassy)
import           Data.Default  (Default, def)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Universum

import           Pos.Crypto    (PublicKey)
import           Pos.Types     (CommitmentsMap, EpochIndex)


data MpcStorage = MpcStorage
    { -- | Local set of Commitments. These are valid Commitments which
      -- are known to the node and not stored in blockchain.  It is
      -- useful only for the first 'k' slots, after that it should be
      -- discarded.
      _mpcLocalCommitments  :: !CommitmentsMap
    , -- | Set of Commitments stored in blocks for current epoch. This
      -- can be calculated by mconcating stored commitments, but it
      -- would be inefficient to do it every time we need to know if
      -- commitments is stored in blocks.
      _mpcGlobalCommitments :: !CommitmentsMap }

makeClassy ''MpcStorage
deriveSafeCopySimple 0 'base ''MpcStorage

instance Default MpcStorage where
    def =
        MpcStorage
        { _mpcLocalCommitments = mempty
        , _mpcGlobalCommitments = mempty
        }

type Update a = forall m x. (HasMpcStorage x, MonadState x m) => m a
type Query a = forall m x. (HasMpcStorage x, MonadReader x m) => m a

-- | Get list of slot leaders for the given epoch. Empty list is returned
-- if no information is available.
getLeaders :: EpochIndex -> Query [PublicKey]
getLeaders _ = pure []
