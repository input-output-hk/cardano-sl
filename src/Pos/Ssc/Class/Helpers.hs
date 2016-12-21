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
       ) where

import           Data.List.NonEmpty    (NonEmpty)
import           Data.Tagged           (Tagged)
import           Serokell.Util.Verify  (VerificationRes)
import           Universum

import           Pos.Crypto            (Threshold, VssPublicKey)
import           Pos.Ssc.Class.Storage (MonadSscGS, SscGlobalQueryM, sscRunGlobalQuery)
import           Pos.Ssc.Class.Types   (Ssc (..))
import           Pos.Types.Types       (MainBlockHeader)
import           Pos.Types.Types       (EpochIndex, SlotLeaders, Utxo)
import           Pos.Util              (AsBinary)

class Ssc ssc => SscHelpersClass ssc where
    sscVerifyPayload :: Tagged ssc (MainBlockHeader ssc -> SscPayload ssc -> VerificationRes)

class Ssc ssc => SscHelpersClassM ssc where
    sscVerifyPayloadM :: Tagged ssc (MainBlockHeader ssc -> SscPayload ssc -> VerificationRes)

    sscGetParticipantsQ :: Word -> Utxo ->
                          SscGlobalQueryM ssc (Maybe (NonEmpty (AsBinary VssPublicKey)))
    sscCalculateLeadersQ :: EpochIndex -> Utxo -> Threshold ->
                           SscGlobalQueryM ssc (Either (SscSeedError ssc)  SlotLeaders)

-- sscGetOurSharesM
--     :: forall ssc m.
--        (MonadSscGS ssc m, SscHelpersClassM ssc)
--     => (AsBinary VssPublicKey) -> m (HashMap Address (AsBinary EncShare))
-- sscGetOurSharesM = sscRunGlobalQuery . sscGetOurSharesQ @ssc
