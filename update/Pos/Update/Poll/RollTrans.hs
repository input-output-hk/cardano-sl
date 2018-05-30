{-# LANGUAGE Rank2Types #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Proxy transformer for tracking updates for
-- rollback. Single-threaded.

module Pos.Update.Poll.RollTrans
       ( RollT
       , runRollT
       , execRollT
       ) where

import           Control.Lens ((%=), (.=))
import           Data.Default (def)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List (find)
import qualified Ether
import           Universum

import           Pos.Binary.Update ()
import           Pos.Core.Update (SoftwareVersion (..))
import           Pos.Crypto (hash)
import           Pos.Update.Poll.Class (MonadPoll (..), MonadPollRead (..))
import           Pos.Update.Poll.Types (PrevValue, USUndo (..), cpsSoftwareVersion, maybeToPrev,
                                        psProposal, unChangedBVL, unChangedConfPropsL,
                                        unChangedPropsL, unChangedSVL, unLastAdoptedBVL,
                                        unPrevProposersL, unSlottingDataL)
import           Pos.Util.Util (ether)

type RollT m = Ether.LazyStateT' USUndo m

-- | Monad transformer which stores USUndo and implements writable
-- MonadPoll. Its purpose is to collect data necessary for rollback.
--
-- [WARNING] This transformer uses StateT and is intended for
-- single-threaded usage only.
instance (MonadPoll m) => MonadPoll (RollT m) where
    putBVState bv sv = ether $ do
        insertIfNotExist bv unChangedBVL getBVState
        putBVState bv sv

    delBVState bv = ether $ do
        insertIfNotExist bv unChangedBVL getBVState
        delBVState bv

    setAdoptedBV = setValueWrapper unLastAdoptedBVL getAdoptedBV setAdoptedBV

    setLastConfirmedSV sv@SoftwareVersion{..} = ether $ do
        insertIfNotExist svAppName unChangedSVL getLastConfirmedSV
        setLastConfirmedSV sv

    -- can't be called during apply
    delConfirmedSV = lift . delConfirmedSV

    addConfirmedProposal cps = ether $ do
        confProps <- getConfirmedProposals
        insertIfNotExist (cpsSoftwareVersion cps) unChangedConfPropsL (getter confProps)
        addConfirmedProposal cps
      where
        getter confs sv = pure $ List.find (\x -> cpsSoftwareVersion x == sv) confs

    -- can't be called during apply
    delConfirmedProposal = lift . delConfirmedProposal

    insertActiveProposal ps = ether $ do
        whenNothingM_ (use unPrevProposersL) $ do
            prev <- getEpochProposers
            unPrevProposersL .= Just prev
        insertIfNotExist (hash $ psProposal $ ps) unChangedPropsL getProposal
        insertActiveProposal ps

    deactivateProposal id = ether $ do
        -- Proposer still can't propose new updates in the current epoch
        -- even if his update was deactivated in the same epoch
        insertIfNotExist id unChangedPropsL getProposal
        deactivateProposal id

    setSlottingData =
        setValueWrapper unSlottingDataL getSlottingData setSlottingData
    setEpochProposers =
        setValueWrapper unPrevProposersL getEpochProposers setEpochProposers

-- This is a convenient wrapper for functions which should set some
-- value and this change should be recorded in USUndo. If change of
-- such kind is already recorded in 'USUndo', then we don't record it
-- and just propagate the new value to the underlying 'MonadPoll'. If
-- it is not recorded, we put old value into 'USUndo' before
-- propagating the new value.
setValueWrapper ::
       MonadPoll m
    => Lens' USUndo (Maybe a)
    -> m a
    -> (a -> m ())
    -> a
    -> RollT m ()
setValueWrapper lens getAction setAction value = ether $ do
    whenNothingM_ (use lens) $ do
        prev <- lift getAction
        lens .= Just prev
    lift (setAction value)

insertIfNotExist
    :: (Eq a, Hashable a, MonadState USUndo m)
    => a
    -> Lens' USUndo (HashMap a (PrevValue b))
    -> (a -> m (Maybe b))
    -> m ()
insertIfNotExist id setter getter = do
    whenNothingM_ (HM.lookup id <$> use setter) $ do
        prev <- getter id
        setter %= HM.insert id (maybeToPrev prev)

runRollT :: RollT m a -> m (a, USUndo)
runRollT = flip Ether.runLazyStateT def

execRollT :: Monad m => RollT m a -> m USUndo
execRollT = flip Ether.execLazyStateT def
