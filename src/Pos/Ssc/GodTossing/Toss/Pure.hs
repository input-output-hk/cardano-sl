-- | Pure Toss.

module Pos.Ssc.GodTossing.Toss.Pure
       ( PureToss (..)
       , MultiRichmenSet
       , runPureToss
       , runPureTossWithLogger
       , evalPureTossWithLogger
       , execPureTossWithLogger
       ) where

import           Control.Lens                   (at, to, (%=), (.=))
import           Control.Monad.RWS.Strict       (RWS, runRWS)
import qualified Data.HashMap.Strict            as HM
import           System.Wlog                    (CanLog, HasLoggerName (getLoggerName),
                                                 LogEvent, LoggerName, LoggerNameBox,
                                                 PureLogger, WithLogger, dispatchEvents,
                                                 logWarning, runPureLog, usingLoggerName)
import           Universum

import           Pos.Lrc.Types                  (RichmenSet)
import           Pos.Ssc.GodTossing.Core        (checkCommShares, checkSharesPure,
                                                 deleteSignedCommitment,
                                                 getCommitmentsMap,
                                                 insertSignedCommitment, vcVssKey)
import           Pos.Ssc.GodTossing.Functions   (computeParticipants)
import           Pos.Ssc.GodTossing.Genesis     (genesisCertificates)
import           Pos.Ssc.GodTossing.Toss.Base   (checkOpeningMatchesCommitment)
import           Pos.Ssc.GodTossing.Toss.Class  (MonadToss (..), MonadTossRead (..))
import           Pos.Ssc.GodTossing.Types       (GtGlobalState, gsCommitments, gsOpenings,
                                                 gsShares, gsVssCertificates)
import qualified Pos.Ssc.GodTossing.VssCertData as VCD
import           Pos.Types                      (EpochIndex, crucialSlot)

type MultiRichmenSet = HashMap EpochIndex RichmenSet

newtype PureToss a = PureToss
    { getPureToss :: LoggerNameBox (PureLogger (RWS MultiRichmenSet () GtGlobalState)) a
    } deriving (Functor, Applicative, Monad, CanLog, HasLoggerName)

instance MonadTossRead PureToss where
    getCommitment id =
        PureToss $ use $ gsCommitments . to getCommitmentsMap . at id
    hasOpening id = PureToss $ use $ gsOpenings . at id . to isJust
    hasShares id = PureToss $ use $ gsShares . at id . to isJust
    hasCertificate id =
        PureToss $ use $ gsVssCertificates . to VCD.certs . at id . to isJust
    getStableCertificates epoch
        | epoch == 0 = pure genesisCertificates
        | otherwise =
            PureToss $
            VCD.certs . VCD.setLastKnownSlot (crucialSlot epoch) <$>
            use gsVssCertificates
    getRichmen epoch = PureToss $ asks (HM.lookup epoch)

    checkCommitmentShares epoch comm = do
        certs <- getStableCertificates epoch
        richmen <- maybe (mempty <$ logWarning "checkShares: no richmen") pure
                         =<< getRichmen epoch
        let parts = map vcVssKey $ toList $ computeParticipants richmen certs
        pure $ checkCommShares parts comm

    matchCommitment op =
        PureToss $ flip checkOpeningMatchesCommitment op <$> use gsCommitments

    checkShares epoch (id, sh) = do
        certs <- getStableCertificates epoch
        richmen <- maybe (mempty <$ logWarning "checkShares: no richmen") pure
                         =<< getRichmen epoch
        let parts = computeParticipants richmen certs
        coms <- PureToss $ use gsCommitments
        ops <- PureToss $ use gsOpenings
        pure $ checkSharesPure coms ops parts id sh

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
    resetCOS = PureToss $ do
        gsCommitments .= mempty
        gsOpenings .= mempty
        gsShares .= mempty
    setEpochOrSlot eos = PureToss $ gsVssCertificates %= VCD.setLastKnownEoS eos

runPureToss
    :: LoggerName
    -> MultiRichmenSet
    -> GtGlobalState
    -> PureToss a
    -> (a, GtGlobalState, [LogEvent])
runPureToss loggerName richmenData gs =
    convertRes .
    (\a -> runRWS a richmenData gs) .
    runPureLog . usingLoggerName loggerName . getPureToss
  where
    convertRes :: ((a, [LogEvent]), GtGlobalState, ())
               -> (a, GtGlobalState, [LogEvent])
    convertRes ((res, logEvents), newGs, ()) = (res, newGs, logEvents)

runPureTossWithLogger
    :: WithLogger m
    => MultiRichmenSet
    -> GtGlobalState
    -> PureToss a
    -> m (a, GtGlobalState)
runPureTossWithLogger richmenData gs action = do
    loggerName <- getLoggerName
    let (res, newGS, events) = runPureToss loggerName richmenData gs action
    (res, newGS) <$ dispatchEvents events

evalPureTossWithLogger
    :: WithLogger m
    => MultiRichmenSet -> GtGlobalState -> PureToss a -> m a
evalPureTossWithLogger r g = fmap fst . runPureTossWithLogger r g

execPureTossWithLogger
    :: WithLogger m
    => MultiRichmenSet -> GtGlobalState -> PureToss a -> m GtGlobalState
execPureTossWithLogger r g = fmap snd . runPureTossWithLogger r g
