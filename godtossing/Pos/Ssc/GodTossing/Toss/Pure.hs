-- | Pure Toss.

module Pos.Ssc.GodTossing.Toss.Pure
       ( PureToss (..)
       , PureTossWithEnv (..)
       , MultiRichmenStakes
       , MultiRichmenSet
       , runPureToss
       , runPureTossWithLogger
       , evalPureTossWithLogger
       , execPureTossWithLogger
       , supplyPureTossEnv
       ) where

import           Universum

import           Control.Lens                   (at, sequenceAOf, (%=), (.=))
import qualified Crypto.Random                  as Rand
import           System.Wlog                    (CanLog, HasLoggerName (..), LogEvent,
                                                 NamedPureLogger (..), WithLogger,
                                                 launchNamedPureLog, runNamedPureLog)

import           Pos.Core                       (BlockVersionData, EpochIndex,
                                                 HasGenesisData, HasProtocolConstants,
                                                 crucialSlot, genesisVssCerts)
import           Pos.Lrc.Types                  (RichmenSet, RichmenStakes)
import           Pos.Ssc.GodTossing.Core        (deleteSignedCommitment,
                                                 insertSignedCommitment)
import           Pos.Ssc.GodTossing.Toss.Class  (MonadToss (..), MonadTossEnv (..),
                                                 MonadTossRead (..))
import           Pos.Ssc.GodTossing.Types       (GtGlobalState, gsCommitments, gsOpenings,
                                                 gsShares, gsVssCertificates)
import qualified Pos.Ssc.GodTossing.VssCertData as VCD

type MultiRichmenStakes = HashMap EpochIndex RichmenStakes
type MultiRichmenSet   = HashMap EpochIndex RichmenSet

-- 'MonadPseudoRandom' is needed because some cryptographic algorithms
-- require randomness even though they are deterministic. Note that running
-- them with the same seed every time is insecure and must not be done.
newtype PureToss a = PureToss
    { getPureToss :: NamedPureLogger (
                     StateT GtGlobalState (
                     Rand.MonadPseudoRandom Rand.ChaChaDRG)) a
    } deriving (Functor, Applicative, Monad,
                CanLog, HasLoggerName, Rand.MonadRandom)

newtype PureTossWithEnv a = PureTossWithEnv
    { getPureTossWithEnv ::
          ReaderT (MultiRichmenStakes, BlockVersionData) PureToss a
    } deriving (Functor, Applicative, Monad, Rand.MonadRandom,
                CanLog, HasLoggerName)

deriving instance (HasProtocolConstants, HasGenesisData) => MonadTossRead PureTossWithEnv
deriving instance (HasProtocolConstants, HasGenesisData) => MonadToss PureTossWithEnv

instance (HasGenesisData, HasProtocolConstants) => MonadTossRead PureToss where
    getCommitments = PureToss $ use gsCommitments
    getOpenings = PureToss $ use gsOpenings
    getShares = PureToss $ use gsShares
    getVssCertificates = PureToss $ VCD.certs <$> use gsVssCertificates
    getStableCertificates epoch
        | epoch == 0 = pure $ genesisVssCerts
        | otherwise =
            PureToss $
            VCD.certs . VCD.setLastKnownSlot (crucialSlot epoch) <$>
            use gsVssCertificates

instance MonadTossEnv PureTossWithEnv where
    getRichmen epoch = PureTossWithEnv $ view (_1 . at epoch)
    getAdoptedBVData = PureTossWithEnv $ view _2

instance (HasProtocolConstants, HasGenesisData) => MonadToss PureToss where
    putCommitment signedComm =
        PureToss $ gsCommitments %= insertSignedCommitment signedComm
    putOpening id op = PureToss $ gsOpenings . at id .= Just op
    putShares id sh = PureToss $ gsShares . at id .= Just sh
    putCertificate cert =
        PureToss $ gsVssCertificates %= VCD.insert cert
    delCommitment id =
        PureToss $ gsCommitments %= deleteSignedCommitment id
    delOpening id = PureToss $ gsOpenings . at id .= Nothing
    delShares id = PureToss $ gsShares . at id .= Nothing
    resetCO = PureToss $ do
        gsCommitments .= mempty
        gsOpenings .= mempty
    resetShares = PureToss $ gsShares .= mempty
    setEpochOrSlot eos = PureToss $ gsVssCertificates %= VCD.setLastKnownEoS eos

runPureToss
    :: Rand.MonadRandom m
    => GtGlobalState
    -> PureToss a
    -> m (a, GtGlobalState, [LogEvent])
runPureToss gs (PureToss act) = do
    seed <- Rand.drgNew
    let ((res, events), newGS) =
            fst . Rand.withDRG seed $    -- run MonadRandom
            (`runStateT` gs) $           -- run State
            runNamedPureLog act          -- run NamedPureLogger
    pure (res, newGS, events)

runPureTossWithLogger
    :: (WithLogger m, Rand.MonadRandom m)
    => GtGlobalState
    -> PureToss a
    -> m (a, GtGlobalState)
runPureTossWithLogger gs (PureToss act) = do
    seed <- Rand.drgNew
    let unwrapLower a = pure $ sequenceAOf _1 $     -- (f a, b) -> f (a, b)
                        fst $ Rand.withDRG seed $   -- run MonadRandom
                        runStateT a gs              -- run State
    (res, newGS) <- launchNamedPureLog unwrapLower act
    return (res, newGS)

evalPureTossWithLogger
    :: (WithLogger m, Rand.MonadRandom m)
    => GtGlobalState
    -> PureToss a
    -> m a
evalPureTossWithLogger g = fmap fst . runPureTossWithLogger g

execPureTossWithLogger
    :: (WithLogger m, Rand.MonadRandom m)
    => GtGlobalState
    -> PureToss a
    -> m GtGlobalState
execPureTossWithLogger g = fmap snd . runPureTossWithLogger g

supplyPureTossEnv
    :: (MultiRichmenStakes, BlockVersionData)
    -> PureTossWithEnv a
    -> PureToss a
supplyPureTossEnv env = flip runReaderT env . getPureTossWithEnv
