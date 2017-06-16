{-# LANGUAGE TypeFamilies #-}

-- | TossT monad transformer. Single-threaded.

module Pos.Ssc.GodTossing.Toss.Trans
       ( TossT
       , runTossT
       , evalTossT
       , execTossT
       ) where

import           Control.Lens                   (at, (%=), (.=))
import qualified Data.HashMap.Strict            as HM
import qualified Ether
import           Mockable                       (ChannelT, Promise, SharedAtomicT,
                                                 ThreadId)
import           Universum

import           Pos.Ssc.GodTossing.Core        (deleteSignedCommitment, getCertId,
                                                 insertSignedCommitment)
import           Pos.Ssc.GodTossing.Toss.Class  (MonadToss (..), MonadTossRead (..))
import           Pos.Ssc.GodTossing.Toss.Types  (TossModifier (..), tmCertificates,
                                                 tmCommitments, tmOpenings, tmShares)
import           Pos.Ssc.GodTossing.VssCertData (VssCertData (certs))
import           Pos.Util.Util                  (ether)

----------------------------------------------------------------------------
-- Tranformer
----------------------------------------------------------------------------

-- | Monad transformer which stores TossModifier and implements
-- writable MonadToss.
--
-- [WARNING] This transformer uses StateT and is intended for
-- single-threaded usage only.
type TossT = Ether.LazyStateT' TossModifier

----------------------------------------------------------------------------
-- Runners
----------------------------------------------------------------------------

runTossT :: TossModifier -> TossT m a -> m (a, TossModifier)
runTossT = flip Ether.runLazyStateT

evalTossT :: Monad m => TossModifier -> TossT m a -> m a
evalTossT = flip Ether.evalLazyStateT

execTossT :: Monad m => TossModifier -> TossT m a -> m TossModifier
execTossT = flip Ether.execLazyStateT

----------------------------------------------------------------------------
-- MonadToss
----------------------------------------------------------------------------

instance MonadTossRead m =>
         MonadTossRead (TossT m) where
    getCommitments = ether $ (<>) <$> use tmCommitments <*> getCommitments
    getOpenings = ether $ (<>) <$> use tmOpenings <*> getOpenings
    getShares = ether $ (<>) <$> use tmShares <*> getShares
    getVssCertificates = certs <$> getVssCertData
    getVssCertData = ether getVssCertData
    getStableCertificates = ether . getStableCertificates
    getRichmen = ether . getRichmen

instance MonadToss m =>
         MonadToss (TossT m) where
    putCommitment signedComm =
        ether $ tmCommitments %= insertSignedCommitment signedComm
    putOpening id op =
        ether $ tmOpenings . at id .= Just op
    putShares id sh =
        ether $ tmShares . at id .= Just sh
    putCertificate cert =
        ether $ tmCertificates %= HM.insert (getCertId cert) cert
    delCommitment id =
        ether $ tmCommitments %= deleteSignedCommitment id
    delOpening id =
        ether $ tmOpenings . at id .= Nothing
    delShares id =
        ether $ tmShares . at id .= Nothing
    resetCO = ether $ do
        tmCommitments .= mempty
        tmOpenings .= mempty
        tmCertificates .= mempty
        resetCO
    resetShares = ether $ do
        tmShares .= mempty
        resetShares
    setEpochOrSlot = ether . setEpochOrSlot

----------------------------------------------------------------------------
-- Common instances used all over the code
----------------------------------------------------------------------------

type instance ThreadId (TossT m) = ThreadId m
type instance Promise (TossT m) = Promise m
type instance SharedAtomicT (TossT m) = SharedAtomicT m
type instance ChannelT (TossT m) = ChannelT m
