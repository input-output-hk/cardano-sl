-- | Pure Toss.

module Pos.Ssc.Toss.Pure
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

import           Universum hiding (id)

import           Control.Lens (at, uses, (%=), (.=))
import qualified Crypto.Random as Rand
import           System.Wlog (CanLog, HasLoggerName (..), LogEvent, NamedPureLogger (..),
                              WithLogger, dispatchEvents, runNamedPureLog)

import           Pos.Core (BlockVersionData, EpochIndex, HasGenesisData, HasProtocolConstants,
                           crucialSlot, genesisVssCerts)
import           Pos.Lrc.Types (RichmenSet, RichmenStakes)
import           Pos.Ssc.Base (deleteSignedCommitment, insertSignedCommitment)
import           Pos.Ssc.Toss.Class (MonadToss (..), MonadTossEnv (..), MonadTossRead (..))
import           Pos.Ssc.Types (SscGlobalState, sgsCommitments, sgsOpenings, sgsShares,
                                sgsVssCertificates)
import qualified Pos.Ssc.VssCertData as VCD

type MultiRichmenStakes = HashMap EpochIndex RichmenStakes
type MultiRichmenSet   = HashMap EpochIndex RichmenSet

-- 'MonadPseudoRandom' is needed because some cryptographic algorithms
-- require randomness even though they are deterministic. Note that running
-- them with the same seed every time is insecure and must not be done.
newtype PureToss a = PureToss
    { getPureToss :: StateT SscGlobalState (
                     NamedPureLogger (
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
    getCommitments = PureToss $ use sgsCommitments
    getOpenings = PureToss $ use sgsOpenings
    getShares = PureToss $ use sgsShares
    getVssCertificates = PureToss $ uses sgsVssCertificates VCD.certs
    getStableCertificates epoch
        | epoch == 0 = pure $ genesisVssCerts
        | otherwise = PureToss $
            uses sgsVssCertificates $
                VCD.certs . VCD.setLastKnownSlot (crucialSlot epoch)

instance MonadTossEnv PureTossWithEnv where
    getRichmen epoch = PureTossWithEnv $ view (_1 . at epoch)
    getAdoptedBVData = PureTossWithEnv $ view _2

instance (HasProtocolConstants, HasGenesisData) => MonadToss PureToss where
    putCommitment signedComm =
        PureToss $ sgsCommitments %= insertSignedCommitment signedComm
    putOpening id op = PureToss $ sgsOpenings . at id .= Just op
    putShares id sh = PureToss $ sgsShares . at id .= Just sh
    putCertificate cert =
        PureToss $ sgsVssCertificates %= VCD.insert cert
    delCommitment id =
        PureToss $ sgsCommitments %= deleteSignedCommitment id
    delOpening id = PureToss $ sgsOpenings . at id .= Nothing
    delShares id = PureToss $ sgsShares . at id .= Nothing
    resetCO = PureToss $ do
        sgsCommitments .= mempty
        sgsOpenings .= mempty
    resetShares = PureToss $ sgsShares .= mempty
    setEpochOrSlot eos = PureToss $ sgsVssCertificates %= VCD.setLastKnownEoS eos

runPureToss
    :: Rand.MonadRandom m
    => SscGlobalState
    -> PureToss a
    -> m (a, SscGlobalState, [LogEvent])
runPureToss gs (PureToss act) = do
    seed <- Rand.drgNew
    let ((res, newGS), events) =
            fst . Rand.withDRG seed $    -- run MonadRandom
            runNamedPureLog $            -- run NamedPureLogger
            runStateT act gs             -- run State
    pure (res, newGS, events)

runPureTossWithLogger
    :: (WithLogger m, Rand.MonadRandom m)
    => SscGlobalState
    -> PureToss a
    -> m (a, SscGlobalState)
runPureTossWithLogger gs act = do
    (res, newGS, events) <- runPureToss gs act
    (res, newGS) <$ dispatchEvents events

evalPureTossWithLogger
    :: (WithLogger m, Rand.MonadRandom m)
    => SscGlobalState
    -> PureToss a
    -> m a
evalPureTossWithLogger g = fmap fst . runPureTossWithLogger g

execPureTossWithLogger
    :: (WithLogger m, Rand.MonadRandom m)
    => SscGlobalState
    -> PureToss a
    -> m SscGlobalState
execPureTossWithLogger g = fmap snd . runPureTossWithLogger g

supplyPureTossEnv
    :: (MultiRichmenStakes, BlockVersionData)
    -> PureTossWithEnv a
    -> PureToss a
supplyPureTossEnv env = flip runReaderT env . getPureTossWithEnv
