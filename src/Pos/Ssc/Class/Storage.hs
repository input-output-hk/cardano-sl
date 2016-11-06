{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Pos.Ssc.Class.Storage
       ( SscStorageClass(..)
       , HasSscStorage(..)

       , SscUpdate
       , SscQuery
       ) where

import           Control.Lens            (Lens')
import           Serokell.Util.Verify    (VerificationRes)
import           Universum

import           Pos.Crypto              (PublicKey, Share, VssKeyPair)
import           Pos.Ssc.Class.Types     (SscTypes (..))
import           Pos.State.Storage.Types (AltChain)
import           Pos.Types.Types         (SlotId)

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

class SscTypes ssc => SscStorageClass ssc where
    -- sscCalculateSeed :: SscQuery ssc (Either (SscSeedError ssc) FtsSeed)

    sscApplyBlocks :: AltChain ssc -> SscUpdate ssc ()
    -- | Should be executed before doing any updates within given slot.
    sscPrepareToNewSlot :: SlotId -> SscUpdate ssc ()
    -- | Do something with given message, result is whether message
    -- has been processed successfully (implementation defined).
    sscProcessMessage :: SscMessage ssc -> SscUpdate ssc Bool
    -- | Rollback application of last 'n' blocks.  blocks. If there
    -- are less blocks than 'n' is, just leaves an empty ('def')
    -- version.
    --
    -- TODO: there was also such comment.
    -- If @n > 0@, also removes all commitments/etc received during that
    -- period but not included into blocks.
    sscRollback :: Word -> SscUpdate ssc ()
    sscGetLocalPayload :: SscQuery ssc (SscPayload ssc)
    sscGetGlobalPayload :: SscQuery ssc (SscPayload ssc)
    sscGetGlobalPayloadByDepth :: Word -> SscQuery ssc (Maybe (SscPayload ssc))
    -- | Verify Ssc-related predicates of block sequence which is
    -- about to be applied. It should check that SSC payload will be
    -- consistent if this blocks are applied (after possible rollback
    -- if first argument isn't zero).
    sscVerifyBlocks :: Word -> AltChain ssc -> SscQuery ssc VerificationRes

    -- | BARDAQ
    sscGetToken :: SscQuery ssc (Maybe (SscToken ssc))
    -- | BARDAQ
    sscSetToken :: SscToken ssc -> SscUpdate ssc ()
    -- | Even more BARDAQ
    sscGetOurShares :: VssKeyPair -> Integer -> SscQuery ssc (HashMap PublicKey Share)

    -- TODO: move the rest of methods here
