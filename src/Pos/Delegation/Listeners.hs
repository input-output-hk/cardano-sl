{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server listeners for delegation logic

module Pos.Delegation.Listeners
       ( delegationRelays
       ) where

import           Universum

import qualified Data.Text.Buildable
import qualified Ether
import           Formatting                    (build, sformat, shown, (%))
import           Serokell.Util.Text            (pairBuilder)
import           System.Wlog                   (logDebug, logInfo)

import           Pos.Binary                    ()
import           Pos.Communication.Limits      ()
import           Pos.Communication.Message     ()
import           Pos.Communication.Relay       (DataParams (..), PropagationMsg (..),
                                                Relay (..), addToRelayQueue)
import           Pos.Communication.Relay.Types (RelayContext)
import           Pos.Context                   (BlkSemaphore (..))
import           Pos.Core                      (getOurKeys)
import           Pos.Crypto                    (SignTag (SignProxySK), proxySign,
                                                pskDelegatePk)
import           Pos.Delegation.Logic          (ConfirmPskLightVerdict (..),
                                                PskHeavyVerdict (..),
                                                PskLightVerdict (..),
                                                processConfirmProxySk,
                                                processProxySKHeavy, processProxySKLight)
import           Pos.Delegation.Types          (ProxySKLightConfirmation)
import           Pos.Types                     (ProxySKHeavy)
import           Pos.WorkMode.Class            (WorkMode)

instance Buildable ProxySKLightConfirmation where
    build = pairBuilder

-- | Listeners for requests related to delegation processing.
delegationRelays
    :: forall ssc m. WorkMode ssc m
    => RelayContext m -> [Relay m]
delegationRelays relayContext =
        [ pskLightRelay relayContext
        , pskHeavyRelay
        , confirmPskRelay
        ]

pskLightRelay
    :: WorkMode ssc m
    => RelayContext m
    -> Relay m
pskLightRelay relayContext = Data $ DataParams $ \provenance pSk -> do
    logDebug $ sformat ("Got request to handle lightweight psk: "%build) pSk
    verdict <- processProxySKLight pSk
    logResult pSk verdict
    case verdict of
        PLUnrelated -> pure True
        PLAdded -> do
           (sk, pk) <- getOurKeys
           if pskDelegatePk pSk == pk then do
               -- if we're final delegate, don't propagate psk, propagate proof instead
               logDebug $
                   sformat ("Generating delivery proof and propagating it to neighbors: "%build) pSk
               let proof = proxySign SignProxySK sk pSk pSk
               -- FIXME seems like a mistake. Shouldn't the relay subsystem
               -- take care of deiciding when to relay?
               addToRelayQueue relayContext (DataOnlyPM (pSk, proof)) provenance
               pure False
           else pure True
        _ -> pure False
  where
    logResult pSk PLAdded =
        logInfo $ sformat ("Got valid related proxy secret key: "%build) pSk
    logResult pSk PLRemoved =
        logInfo $
        sformat ("Removing keys from issuer because got "%
                 "self-signed revocation: "%build) pSk
    logResult _ verdict =
        logDebug $
        sformat ("Got proxy signature that wasn't accepted. Reason: "%shown) verdict

pskHeavyRelay
    :: WorkMode ssc m
    => Relay m
pskHeavyRelay = Data $ DataParams $ \_ -> handlePsk
  where
    handlePsk :: forall ssc m. WorkMode ssc m => ProxySKHeavy -> m Bool
    handlePsk pSk = do
        logDebug $ sformat ("Got request to handle heavyweight psk: "%build) pSk
        verdict <- processProxySKHeavy @ssc pSk
        logDebug $ sformat ("The verdict for cert "%build%" is: "%shown) pSk verdict
        case verdict of
            PHIncoherent -> do
                -- We're probably updating state over epoch, so leaders
                -- can be calculated incorrectly.
                blkSemaphore <- Ether.asks' unBlkSemaphore
                void $ readMVar blkSemaphore
                handlePsk pSk
            PHAdded -> pure True
            _ -> pure False

confirmPskRelay
    :: WorkMode ssc m
    => Relay m
confirmPskRelay = Data $ DataParams $ \_ (pSk, proof) -> do
    verdict <- processConfirmProxySk pSk proof
    pure $ case verdict of
        CPValid -> True
        _       -> False
