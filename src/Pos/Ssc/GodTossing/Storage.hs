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

-- | Dynamic State storage.

module Pos.Ssc.GodTossing.Storage
       ( GtStorage (..)
       , HasGtSecretStorage(..)
       , GtStorageVersion (..)
       , GtSecretStorageClass (..)
       , GtSecretStorage(..)
       , GtSecret
       , SecQuery
       , SecUpdate

       -- * Lenses
       -- ** GtStorage
       , dsVersionedL
       , dsLastProcessedSlotL
       -- ** GtSecretStorage
       , dsCurrentSecretL
       , dsSecLastProcessedSlotL
       -- ** GtStorageVersion
       , dsLocalCommitments
       , dsGlobalCommitments
       , dsLocalShares
       , dsGlobalShares
       , dsLocalOpenings
       , dsGlobalOpenings
       , dsLocalCertificates
       , dsGlobalCertificates
       ) where

import           Control.Lens               (Lens')
import           Control.Lens               (makeLenses, makeLensesFor)
import           Data.Default               (Default (..))
import           Data.List.NonEmpty         (NonEmpty (..))
import           Data.List.NonEmpty         (NonEmpty ((:|)))
import           Data.SafeCopy              (base, deriveSafeCopySimple)
import           Universum

import           Pos.Crypto                 (PublicKey)
import           Pos.Ssc.GodTossing.Base    (CommitmentsMap, Opening, OpeningsMap,
                                             SharesMap, SignedCommitment,
                                             VssCertificatesMap)
import           Pos.Ssc.GodTossing.Genesis (genesisCertificates)
import           Pos.Types                  (SlotId, unflattenSlotId)

data GtStorageVersion = GtStorageVersion
    { -- | Local set of 'Commitment's. These are valid commitments which are
      -- known to the node and not stored in blockchain. It is useful only
      -- for the first 'k' slots, after that it should be discarded.
      _dsLocalCommitments   :: !CommitmentsMap
    , -- | Set of 'Commitment's stored in blocks for current epoch. This can
      -- be calculated by 'mconcat'ing stored commitments, but it would be
      -- inefficient to do it every time we need to know if commitments is
      -- stored in blocks.
      _dsGlobalCommitments  :: !CommitmentsMap
    , -- | Local set of decrypted shares (encrypted shares are stored in
      -- commitments).
      _dsLocalShares        :: !SharesMap
    , -- | Decrypted shares stored in blocks. These shares are guaranteed to
      -- match encrypted shares stored in 'dsGlobalCommitments'.
      _dsGlobalShares       :: !SharesMap
    , -- | Local set of openings
      _dsLocalOpenings      :: !OpeningsMap
    , -- | Openings stored in blocks
      _dsGlobalOpenings     :: !OpeningsMap
    , -- | Local set of VSS certificates
      _dsLocalCertificates  :: !VssCertificatesMap
    , -- | VSS certificates stored in blocks (for all time, not just for
      -- current epoch)
      _dsGlobalCertificates :: !VssCertificatesMap }
      deriving Show

makeLenses ''GtStorageVersion
deriveSafeCopySimple 0 'base ''GtStorageVersion

instance Default GtStorageVersion where
    def =
        GtStorageVersion
        { _dsLocalCommitments = mempty
        , _dsGlobalCommitments = mempty
        , _dsLocalShares = mempty
        , _dsGlobalShares = mempty
        , _dsLocalOpenings = mempty
        , _dsGlobalOpenings = mempty
        , _dsLocalCertificates = mempty
        , _dsGlobalCertificates = genesisCertificates
        }

data GtStorage = GtStorage
    { -- | Last several versions of MPC storage, a version for each received
      -- block. To bring storage to the state as it was just before the last
      -- block arrived, just remove the head. All incoming commitments/etc
      -- which aren't parts of blocks are applied to the head, too.
      --
      -- TODO: this is a very naive solution. A better one would be storing
      -- deltas for maps which are in 'GtStorageVersion', see [POS-25] for
      -- the explanation of deltas.
      _dsVersioned         :: NonEmpty GtStorageVersion
    , -- | Last slot we are aware of.
      _dsLastProcessedSlot :: !SlotId
    }


flip makeLensesFor ''GtStorage
    [ ("_dsVersioned", "dsVersionedL")
    , ("_dsLastProcessedSlot", "dsLastProcessedSlotL")
    ]
deriveSafeCopySimple 0 'base ''GtStorage

type GtSecret = (PublicKey, SignedCommitment, Opening)
data GtSecretStorage = GtSecretStorage
    {
      -- | Secret that we are using for the current epoch.
      _dsCurrentSecret        :: !(Maybe GtSecret)
    , -- | Last slot we are aware of.
      _dsSecLastProcessedSlot :: !SlotId
    }

class HasGtSecretStorage ss a where
    secretStorage :: Lens' a ss

type SecQuery a = forall m x . (HasGtSecretStorage GtSecretStorage x, MonadReader x m) => m a
type SecUpdate a = forall m x . (HasGtSecretStorage GtSecretStorage x, MonadState x m) => m a

class GtSecretStorageClass ss where
    ssGetSecret :: SecQuery (Maybe GtSecret)
    ssSetSecret :: GtSecret -> SecUpdate ()
    ssPrepareToNewSlot :: SlotId -> SecUpdate ()

flip makeLensesFor ''GtSecretStorage
    [
      ("_dsCurrentSecret", "dsCurrentSecretL")
    , ("_dsSecLastProcessedSlot", "dsSecLastProcessedSlotL")
    ]
deriveSafeCopySimple 0 'base ''GtSecretStorage

instance Default GtStorage where
    def =
        GtStorage
        { _dsVersioned = (def :| [])
        , _dsLastProcessedSlot = unflattenSlotId 0
        }

instance Default GtSecretStorage where
    def =
        GtSecretStorage
        {
          _dsCurrentSecret = Nothing
        , _dsSecLastProcessedSlot = unflattenSlotId 0
        }

