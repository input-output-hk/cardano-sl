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
       , pureTossTrace
       , pureTossWithEnvTrace
       ) where

import           Universum hiding (forM_, id)

import           Control.Lens (at, uses, (%=), (.=))
import           Control.Monad (forM_)
import           Control.Monad.Trans.Writer (WriterT (..))
import qualified Crypto.Random as Rand
import           Data.DList (DList)
import qualified Data.DList as DList
import           Data.Functor.Contravariant (contramap)

import           Pos.Core (BlockVersionData, EpochIndex, HasGenesisData,
                     HasProtocolConstants, crucialSlot, genesisVssCerts)
import           Pos.Lrc.Types (RichmenSet, RichmenStakes)
import           Pos.Ssc.Base (deleteSignedCommitment, insertSignedCommitment)
import           Pos.Ssc.Toss.Class (MonadToss (..), MonadTossEnv (..),
                     MonadTossRead (..))
import           Pos.Ssc.Types (SscGlobalState, sgsCommitments, sgsOpenings,
                     sgsShares, sgsVssCertificates)
import qualified Pos.Ssc.VssCertData as VCD
import           Pos.Util.Trace (Trace, natTrace, traceWith)
import           Pos.Util.Trace.Unstructured (LogItem)
import           Pos.Util.Trace.Writer (writerTrace)

type MultiRichmenStakes = HashMap EpochIndex RichmenStakes
type MultiRichmenSet   = HashMap EpochIndex RichmenSet

-- 'MonadPseudoRandom' is needed because some cryptographic algorithms
-- require randomness even though they are deterministic. Note that running
-- them with the same seed every time is insecure and must not be done.
newtype PureToss a = PureToss
    { getPureToss :: StateT SscGlobalState (
                     WriterT (DList LogItem) (
                     Rand.MonadPseudoRandom Rand.ChaChaDRG)) a
    } deriving (Functor, Applicative, Monad, Rand.MonadRandom)

newtype PureTossWithEnv a = PureTossWithEnv
    { getPureTossWithEnv ::
          ReaderT (MultiRichmenStakes, BlockVersionData) PureToss a
    } deriving (Functor, Applicative, Monad, Rand.MonadRandom)

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



pureTossTrace :: Trace PureToss LogItem
pureTossTrace = contramap DList.singleton (natTrace PureToss writerTrace)

pureTossWithEnvTrace :: Trace PureTossWithEnv LogItem
pureTossWithEnvTrace = natTrace (PureTossWithEnv . ReaderT . const) pureTossTrace

runPureToss
    :: Rand.MonadRandom m
    => SscGlobalState
    -> PureToss a
    -> m (a, SscGlobalState , DList LogItem)
runPureToss gs (PureToss act) = do
    seed <- Rand.drgNew
    let ((res, newGS) , events) =
            fst . Rand.withDRG seed $    -- run MonadRandom
            runWriterT $                 -- run the DList Writer
            runStateT act gs             -- run State
    pure (res, newGS , events)


runPureTossWithLogger
    :: Rand.MonadRandom m
    => SscGlobalState
    -> Trace m LogItem
    -> PureToss a
    -> m (a, SscGlobalState)
runPureTossWithLogger gs logTrace act = do
    (res, newGS , events ) <- runPureToss gs act
    (res, newGS) <$ (forM_ events (traceWith logTrace))

evalPureTossWithLogger
    :: Rand.MonadRandom m
    => SscGlobalState
    -> Trace m LogItem
    -> PureToss a
    -> m a
evalPureTossWithLogger g logTrace = fmap fst . runPureTossWithLogger g logTrace

execPureTossWithLogger
    :: Rand.MonadRandom m
    => SscGlobalState
    -> Trace m LogItem
    -> PureToss a
    -> m SscGlobalState
execPureTossWithLogger g logTrace = fmap snd . runPureTossWithLogger g logTrace

supplyPureTossEnv
    :: (MultiRichmenStakes, BlockVersionData)
    -> PureTossWithEnv a
    -> PureToss a
supplyPureTossEnv env = flip runReaderT env . getPureTossWithEnv
