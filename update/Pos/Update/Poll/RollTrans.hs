{-# LANGUAGE Rank2Types #-}
-- | Proxy transformer for tracking updates for
-- rollback. Single-threaded.

module Pos.Update.Poll.RollTrans
       ( RollT
       , runRollT
       , execRollT
       ) where

import           Control.Lens          ((%=), (.=))
import           Data.Default          (def)
import qualified Data.HashMap.Strict   as HM
import qualified Data.List             as List (find)
import qualified Ether
import           Universum

import           Pos.Binary.Update     ()
import           Pos.Core.Types        (SoftwareVersion (..))
import           Pos.Crypto            (hash)
import           Pos.Update.Poll.Class (MonadPoll (..), MonadPollRead (..))
import           Pos.Update.Poll.Types (PrevValue, USUndo (..), cpsSoftwareVersion,
                                        maybeToPrev, psProposal, unChangedBVL,
                                        unChangedConfPropsL, unChangedPropsL,
                                        unChangedSVL, unLastAdoptedBVL, unPrevProposersL)
import           Pos.Util.Util         (ether)

type RollT m = Ether.LazyStateT' USUndo m

-- | Monad transformer which stores USUndo and implements writable
-- MonadPoll. Its purpose is to collect data necessary for rollback.
--
-- [WARNING] This transformer uses StateT and is intended for
-- single-threaded usage only.
instance MonadPoll m => MonadPoll (RollT m) where
    putBVState bv sv = ether $ do
        insertIfNotExist bv unChangedBVL getBVState
        putBVState bv sv

    delBVState bv = ether $ do
        insertIfNotExist bv unChangedBVL getBVState
        delBVState bv

    setAdoptedBV pv = ether $ do
        prevBV <- getAdoptedBV
        whenNothingM_ (use unLastAdoptedBVL) $
            unLastAdoptedBVL .= Just prevBV
        setAdoptedBV pv

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

    setEpochProposers proposers = ether $ do
        whenNothingM_ (use unPrevProposersL) $ do
            prev <- getEpochProposers
            unPrevProposersL .= Just prev
        setEpochProposers proposers

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
