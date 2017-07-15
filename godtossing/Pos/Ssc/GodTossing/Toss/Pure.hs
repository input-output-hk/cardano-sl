-- | Pure Toss.

module Pos.Ssc.GodTossing.Toss.Pure
       ( PureToss (..)
       , PureTossWithEnv (..)
       , MultiRichmenStake
       , MultiRichmenSet
       , runPureToss
       , runPureTossWithLogger
       , evalPureTossWithLogger
       , execPureTossWithLogger
       , supplyPureTossEnv
       ) where

import           Control.Lens                   (at, sequenceAOf, (%=), (.=))
import           System.Wlog                    (CanLog, HasLoggerName (..), LogEvent,
                                                 NamedPureLogger (..), WithLogger,
                                                 launchNamedPureLog, runNamedPureLog)
import           Universum

import           Pos.Core                       (BlockVersionData, EpochIndex,
                                                 crucialSlot)
import           Pos.Lrc.Types                  (RichmenSet, RichmenStake)
import           Pos.Ssc.GodTossing.Core        (deleteSignedCommitment,
                                                 insertSignedCommitment)
import           Pos.Ssc.GodTossing.Genesis     (genesisCertificates)
import           Pos.Ssc.GodTossing.Toss.Class  (MonadToss (..), MonadTossEnv (..),
                                                 MonadTossRead (..))
import           Pos.Ssc.GodTossing.Types       (GtGlobalState, gsCommitments, gsOpenings,
                                                 gsShares, gsVssCertificates)
import qualified Pos.Ssc.GodTossing.VssCertData as VCD

type MultiRichmenStake = HashMap EpochIndex RichmenStake
type MultiRichmenSet   = HashMap EpochIndex RichmenSet

newtype PureToss a = PureToss
    { getPureToss ::
          NamedPureLogger (State GtGlobalState) a
    } deriving (Functor, Applicative, Monad, CanLog, HasLoggerName)

newtype PureTossWithEnv a = PureTossWithEnv
    { getPureTossWithEnv ::
          ReaderT (MultiRichmenStake, BlockVersionData) PureToss a
    } deriving (Functor, Applicative, Monad, CanLog, HasLoggerName,
                MonadTossRead, MonadToss)

instance MonadTossRead PureToss where
    getCommitments = PureToss $ use gsCommitments
    getOpenings = PureToss $ use gsOpenings
    getShares = PureToss $ use gsShares
    getVssCertificates = PureToss $ VCD.certs <$> use gsVssCertificates
    getStableCertificates epoch
        | epoch == 0 = pure genesisCertificates
        | otherwise =
            PureToss $
            VCD.certs . VCD.setLastKnownSlot (crucialSlot epoch) <$>
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

runPureToss
    :: GtGlobalState
    -> PureToss a
    -> (a, GtGlobalState, [LogEvent])
runPureToss gs =
    reorder . (`runState` gs) . runNamedPureLog . getPureToss
  where
    reorder :: ((a, [LogEvent]), GtGlobalState)
            -> (a, GtGlobalState, [LogEvent])
    reorder ((res, logEvents), newGs) = (res, newGs, logEvents)

runPureTossWithLogger
    :: WithLogger m
    => GtGlobalState
    -> PureToss a
    -> m (a, GtGlobalState)
runPureTossWithLogger gs action = do
    let unwrapLower a = return $ sequenceAOf _1 $ runState a gs
    (res, newGS) <- launchNamedPureLog unwrapLower $ getPureToss action
    return (res, newGS)

evalPureTossWithLogger
    :: WithLogger m
    => GtGlobalState
    -> PureToss a
    -> m a
evalPureTossWithLogger g = fmap fst . runPureTossWithLogger g

execPureTossWithLogger
    :: WithLogger m
    => GtGlobalState
    -> PureToss a
    -> m GtGlobalState
execPureTossWithLogger g = fmap snd . runPureTossWithLogger g

supplyPureTossEnv
    :: (MultiRichmenStake, BlockVersionData)
    -> PureTossWithEnv a
    -> PureToss a
supplyPureTossEnv env = flip runReaderT env . getPureTossWithEnv
