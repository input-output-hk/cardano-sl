{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | TossT monad transformer. Single-threaded.

module Pos.Chain.Ssc.Toss.Trans
       ( TossT
       , runTossT
       , evalTossT
       , execTossT
       ) where

import           Universum hiding (id)

import           Control.Lens (at, (%=), (.=))
import qualified Ether

import           Pos.Chain.Ssc.Base (deleteSignedCommitment,
                     insertSignedCommitment)
import           Pos.Chain.Ssc.Toss.Class (MonadToss (..), MonadTossEnv (..),
                     MonadTossRead (..))
import           Pos.Chain.Ssc.Toss.Types (TossModifier (..), tmCertificates,
                     tmCommitments, tmOpenings, tmShares)
import           Pos.Core.Ssc (insertVss)
import           Pos.Util.Util (ether)

----------------------------------------------------------------------------
-- Tranformer
----------------------------------------------------------------------------

-- | Monad transformer which stores TossModifier and implements
-- writable MonadToss.
--
-- [WARNING] This transformer uses StateT and is intended for
-- single-threaded usage only.
type TossT = Ether.StateT' TossModifier

----------------------------------------------------------------------------
-- Runners
----------------------------------------------------------------------------

runTossT :: TossModifier -> TossT m a -> m (a, TossModifier)
runTossT = flip Ether.runStateT

evalTossT :: Monad m => TossModifier -> TossT m a -> m a
evalTossT = flip Ether.evalStateT

execTossT :: Monad m => TossModifier -> TossT m a -> m TossModifier
execTossT = flip Ether.execStateT

----------------------------------------------------------------------------
-- MonadToss
----------------------------------------------------------------------------

instance MonadTossRead m =>
         MonadTossRead (TossT m) where
    getCommitments = ether $ (<>) <$> use tmCommitments <*> getCommitments
    getOpenings = ether $ (<>) <$> use tmOpenings <*> getOpenings
    getShares = ether $ (<>) <$> use tmShares <*> getShares
    getVssCertificates = ether $ (<>) <$> use tmCertificates <*> getVssCertificates
    getStableCertificates = ether . getStableCertificates

instance MonadTossEnv m =>
         MonadTossEnv (TossT m) where
    getRichmen = ether . getRichmen
    getAdoptedBVData = ether getAdoptedBVData

instance MonadToss m =>
         MonadToss (TossT m) where
    putCommitment signedComm =
        ether $ tmCommitments %= insertSignedCommitment signedComm
    putOpening id op =
        ether $ tmOpenings . at id .= Just op
    putShares id sh =
        ether $ tmShares . at id .= Just sh
    -- NB. 'insertVss' might delete some certs from the map, but it
    -- shouldn't actually happen in practice because
    -- 'checkCertificatesPayload' ensures that there are no clashes between
    -- the certificates in blocks and certificates in the map
    putCertificate cert =
        ether $ tmCertificates %= fst . insertVss cert
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
