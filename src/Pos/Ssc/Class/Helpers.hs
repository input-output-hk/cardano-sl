{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Pos.Ssc.Class.Helpers
       (
         SscHelpersClass (..)
       , SscHelpersClassM (..)
       , SscQuery
       , sscGetOurShares
       ) where

import           Data.List.NonEmpty    (NonEmpty)
import           Data.Tagged           (Tagged)
import           Serokell.Util.Verify  (VerificationRes)
import           Universum

import           Pos.Crypto            (EncShare, Threshold, VssPublicKey)
import           Pos.Ssc.Class.Storage (MonadSscGS, sscRunGlobalQuery)
import           Pos.Ssc.Class.Types   (Ssc (..))
import           Pos.Types.Types       (MainBlockHeader)
import           Pos.Types.Types       (Address, EpochIndex, SlotLeaders, Utxo)
import           Pos.Util              (AsBinary)

-- | Generic @SSC@ query.
type SscQuery ssc a =
    forall m . (MonadReader (SscGlobalState ssc) m) => m a

class Ssc ssc => SscHelpersClass ssc where
    sscVerifyPayload :: Tagged ssc (MainBlockHeader ssc -> SscPayload ssc -> VerificationRes)

class Ssc ssc => SscHelpersClassM ssc where
    sscVerifyPayloadQ :: Tagged ssc (MainBlockHeader ssc -> SscPayload ssc -> VerificationRes)

    sscGetOurSharesQ
        :: (AsBinary VssPublicKey)
        -> SscQuery ssc (HashMap Address (AsBinary EncShare))

    sscGetParticipantsQ :: Word -> Utxo ->
                          SscQuery ssc (Maybe (NonEmpty (AsBinary VssPublicKey)))
    sscCalculateLeadersQ :: EpochIndex -> Utxo -> Threshold ->
                           SscQuery ssc (Either (SscSeedError ssc)  SlotLeaders)

sscGetOurShares
    :: forall ssc m.
       (MonadSscGS ssc m, SscHelpersClassM ssc)
    => (AsBinary VssPublicKey) -> m (HashMap Address (AsBinary EncShare))
sscGetOurShares = sscRunGlobalQuery . sscGetOurSharesQ @ssc
