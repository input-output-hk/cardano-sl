{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Pos.Ssc.Class.Storage
       ( SscStorageClass(..)
       , HasSscStorage(..)

       , SscUpdate
       , SscQuery
       , SscStorageMode
       ) where

import           Control.Lens            (Lens')
import           Data.List.NonEmpty      (NonEmpty)
import           Data.SafeCopy           (SafeCopy)
import           Data.Tagged             (Tagged)
import           Serokell.Util.Verify    (VerificationRes)
import           Universum

import           Pos.Crypto              (PublicKey, Share, Threshold, VssKeyPair,
                                          VssPublicKey)
import           Pos.Ssc.Class.Types     (Ssc (..))
import           Pos.State.Storage.Types (AltChain)
import           Pos.Types.Types         (EpochIndex, MainBlockHeader, SlotLeaders, Utxo)

type SscUpdate ssc a =
    forall m x. (HasSscStorage ssc x, MonadState x m) => m a

-- If this type ever changes to include side effects (error reporting, etc)
-- we might have to change 'mpcVerifyBlock' because currently it works by
-- simulating block application and we don't want block verification to have
-- any side effects. The compiler will warn us if it happens, though.
type SscQuery ssc a =
    forall m x. (HasSscStorage ssc x, MonadReader x m) => m a

class HasSscStorage ssc a where
    sscStorage :: Lens' a (SscStorage ssc)

class Ssc ssc => SscStorageClass ssc where
    -- sscCalculateSeed :: SscQuery ssc (Either (SscSeedError ssc) SharedSeed)

    sscApplyBlocks :: AltChain ssc -> SscUpdate ssc ()
    -- | Rollback application of last 'n' blocks.  blocks. If there
    -- are less blocks than 'n' is, just leaves an empty ('def')
    -- version.
    --
    -- TODO: there was also such comment.
    -- If @n > 0@, also removes all commitments/etc received during that
    -- period but not included into blocks.
    sscRollback :: Word -> SscUpdate ssc ()
    sscGetGlobalPayload :: SscQuery ssc (SscPayload ssc)
    sscGetGlobalPayloadByDepth :: Word -> SscQuery ssc (Maybe (SscPayload ssc))
    -- | Verify Ssc-related predicates of block sequence which is
    -- about to be applied. It should check that SSC payload will be
    -- consistent if this blocks are applied (after possible rollback
    -- if first argument isn't zero).
    sscVerifyBlocks :: Word -> AltChain ssc -> SscQuery ssc VerificationRes

    -- | BARDAQ
    sscGetOurShares :: VssKeyPair -> Integer -> SscQuery ssc (HashMap PublicKey Share)

    -- TODO: yet another BARDAQ
    sscGetParticipants :: Word -> Utxo ->
                          SscQuery ssc (Maybe (NonEmpty VssPublicKey))
    sscCalculateLeaders :: EpochIndex -> Utxo -> Threshold ->
                           SscQuery ssc (Either (SscSeedError ssc)  SlotLeaders)

    -- TODO: one more BARDAQ. It's not related to Storage, but can't
    -- be put into Ssc now :(
    -- | Verify payload using header containing this payload.
    sscVerifyPayload :: Tagged ssc (MainBlockHeader ssc -> SscPayload ssc -> VerificationRes)

type SscStorageMode ssc = (SscStorageClass ssc, SafeCopy ssc)
