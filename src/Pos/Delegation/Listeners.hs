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
import           Pos.Communication.Relay.Types ()
import           Pos.Context                   (BlkSemaphore (..), NodeParams,
                                                npSecretKey)
import           Pos.Crypto                    (SignTag (SignProxySK), proxySign)
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
    => [Relay m]
delegationRelays =
        [ pskLightRelay
        , pskHeavyRelay
        , confirmPskRelay
        ]

pskLightRelay
    :: WorkMode ssc m
    => Relay m
pskLightRelay = Data $ DataParams $ \pSk -> do
    -- do it in worker once in ~sometimes instead of on every request
    verdict <- processProxySKLight pSk
    logResult pSk verdict
    case verdict of
        PLUnrelated -> return True
        PLAdded -> do
           logDebug $
               sformat ("Generating delivery proof and propagating it to neighbors: "%build) pSk
           sk <- npSecretKey <$> Ether.ask @NodeParams
           let proof = proxySign SignProxySK sk pSk pSk -- but still proving is
                                                        -- nothing but fear
           addToRelayQueue (DataOnlyPM (pSk, proof))

           -- Broadcasted further for case we have multiple nodes up with same secret key
           return True
        _ -> return False
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
pskHeavyRelay = Data $ DataParams $ handlePsk
  where
    handlePsk :: forall ssc m. WorkMode ssc m => ProxySKHeavy -> m Bool
    handlePsk pSk = do
        logDebug $ sformat ("GLightLightot request to handle heavyweight psk: "%build) pSk
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
confirmPskRelay = Data $ DataParams $ \(pSk, proof) -> do
    verdict <- processConfirmProxySk pSk proof
    pure $ case verdict of
        CPValid -> True
        _       -> False

--handleCheckProxySKConfirmed
--    :: forall ssc m.
--       (WorkMode ssc m)
--    => (ListenerSpec m, OutSpecs)
--handleCheckProxySKConfirmed = listenerOneMsg outSpecs $
--    \_ nodeId sendActions (CheckProxySKConfirmed pSk :: CheckProxySKConfirmed) -> do
--        logDebug $ sformat ("Got request to check if psk: "%build%" was delivered.") pSk
--        res <- runDelegationStateAction $ isProxySKConfirmed pSk
--        sendTo sendActions nodeId $ CheckProxySKConfirmedRes res
--  where
--    outSpecs = toOutSpecs [oneMsgH (Proxy :: Proxy CheckProxySKConfirmedRes)]
