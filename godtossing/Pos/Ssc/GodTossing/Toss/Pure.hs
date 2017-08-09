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
import           Control.Monad.Reader           (mapReaderT)
import           Control.Monad.RWS.Strict       (RWS, runRWS)
import           System.Wlog                    (CanLog, HasLoggerName (..), LogEvent,
                                                 NamedPureLogger (..), WithLogger,
                                                 launchNamedPureLog, runNamedPureLog)

import           Pos.Core                       (BlockVersionData, CoreConstants,
                                                 EpochIndex, HasCoreConstants (..),
                                                 ccBlkSecuriryParam, crucialSlot)
import           Pos.Lrc.Types                  (RichmenSet, RichmenStakes)
import           Pos.Ssc.GodTossing.Core        (deleteSignedCommitment,
                                                 insertSignedCommitment)
import           Pos.Ssc.GodTossing.Genesis     (genesisCertificates)
import           Pos.Ssc.GodTossing.Toss.Class  (MonadToss (..), MonadTossEnv (..),
                                                 MonadTossRead (..))
import           Pos.Ssc.GodTossing.Types       (GtGlobalState, gsCommitments, gsOpenings,
                                                 gsShares, gsVssCertificates)
import qualified Pos.Ssc.GodTossing.VssCertData as VCD

type MultiRichmenStakes = HashMap EpochIndex RichmenStakes
type MultiRichmenSet   = HashMap EpochIndex RichmenSet

newtype PureToss a = PureToss
    { getPureToss ::
          NamedPureLogger (RWS CoreConstants () GtGlobalState) a
    } deriving (Functor, Applicative, Monad, CanLog, HasLoggerName, MonadReader CoreConstants)

newtype PureTossWithEnv a = PureTossWithEnv
    { getPureTossWithEnv ::
          ReaderT (MultiRichmenStakes, BlockVersionData) PureToss a
    } deriving (Functor, Applicative, Monad, CanLog, HasLoggerName,
                MonadTossRead, MonadToss)

instance MonadReader CoreConstants PureTossWithEnv where
    ask = PureTossWithEnv $ lift ask
    local f = PureTossWithEnv . mapReaderT (local f) . getPureTossWithEnv

instance MonadTossRead PureToss where
    getCommitments = PureToss $ use gsCommitments
    getOpenings = PureToss $ use gsOpenings
    getShares = PureToss $ use gsShares
    getVssCertificates = PureToss $ VCD.certs <$> use gsVssCertificates
    getStableCertificates epoch
        | epoch == 0 = pure genesisCertificates
        | otherwise =
            PureToss $ do
                k <- view ccBlkSecuriryParam
                VCD.certs . VCD.setLastKnownSlot (crucialSlot k epoch) <$>
                    use gsVssCertificates

instance MonadTossEnv PureTossWithEnv where
    getRichmen epoch = PureTossWithEnv $ view (_1 . at epoch)
    getAdoptedBVData = PureTossWithEnv $ view _2

instance MonadToss PureToss where
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

runPureToss ::
       CoreConstants
    -> GtGlobalState
    -> PureToss a
    -> (a, GtGlobalState, [LogEvent])
runPureToss constants gs =
    reorder . (\rws -> runRWS rws constants gs) . runNamedPureLog . getPureToss
  where
    reorder :: ((a, [LogEvent]), GtGlobalState, ())
            -> (a, GtGlobalState, [LogEvent])
    reorder ((res, logEvents), newGS, ()) = (res, newGS, logEvents)

runPureTossWithLogger
    :: (WithLogger m, MonadReader ctx m, HasCoreConstants ctx)
    => GtGlobalState
    -> PureToss a
    -> m (a, GtGlobalState)
runPureTossWithLogger gs action = do
    constants <- view coreConstantsG
    let discardOutput (a, s, ()) = (a, s)
    let unwrapLower a =
            return $ sequenceAOf _1 $ discardOutput $ runRWS a constants gs
    (res, newGS) <- launchNamedPureLog unwrapLower $ getPureToss action
    return (res, newGS)

evalPureTossWithLogger
    :: (WithLogger m, MonadReader ctx m, HasCoreConstants ctx)
    => GtGlobalState
    -> PureToss a
    -> m a
evalPureTossWithLogger g = fmap fst . runPureTossWithLogger g

execPureTossWithLogger
    :: (WithLogger m, MonadReader ctx m, HasCoreConstants ctx)
    => GtGlobalState
    -> PureToss a
    -> m GtGlobalState
execPureTossWithLogger g = fmap snd . runPureTossWithLogger g

supplyPureTossEnv
    :: (MultiRichmenStakes, BlockVersionData)
    -> PureTossWithEnv a
    -> PureToss a
supplyPureTossEnv env = flip runReaderT env . getPureTossWithEnv
