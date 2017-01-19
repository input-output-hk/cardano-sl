-- | Functions which work in MonadUSMem.

module Pos.Update.MemState.Functions
       ( withUSLock
       , modifyMemPool
       , modifyPollModifier
       ) where

import qualified Control.Concurrent.Lock      as Lock
import           Control.Monad.Catch          (MonadMask, bracket_)
import qualified Data.HashMap.Strict          as HM
import qualified Data.HashSet                 as HS
import           Universum

import           Pos.Crypto                   (hash)
import           Pos.Update.Core.Types        (UpdatePayload (..), UpdateVote (..))
import           Pos.Update.MemState.Class    (MonadUSMem (askUSMemVar))
import           Pos.Update.MemState.MemState (MemVar (..))
import           Pos.Update.MemState.Types    (MemPool (..))
import           Pos.Update.Poll.Types        (PollModifier (..), psProposal, psVotes)

withUSLock
    :: (MonadUSMem m, MonadIO m, MonadMask m)
    => m a -> m a
withUSLock action = do
    lock <- mvLock <$> askUSMemVar
    bracket_ (liftIO $ Lock.acquire lock) (liftIO $ Lock.release lock) action

modifyMemPool :: UpdatePayload -> PollModifier -> MemPool -> MemPool
modifyMemPool UpdatePayload {..} PollModifier{..} =
     addModifiers . delModifiers . addProposal upProposal
  where
    delModifiers MemPool{..} = MemPool
        (foldr' HM.delete mpProposals pmDelActiveProps)
        (foldr' HM.delete mpGlobalVotes pmDelActiveProps)
    addModifiers MemPool{..} = MemPool
        (foldr' (uncurry HM.insert) mpProposals
             (HM.toList $ HM.map psProposal pmNewActiveProps))
        (foldr' insertVote mpGlobalVotes .
             mapMaybe (\x -> (x,) <$> lookupVS pmNewActiveProps x) $ upVotes)
    addProposal Nothing  mp = mp
    addProposal (Just p) MemPool {..} = MemPool
        (HM.insert (hash p) p mpProposals)
        mpGlobalVotes
    lookupVS activeProps UpdateVote{..} =
        HM.lookup uvProposalId activeProps >>= HM.lookup uvKey . psVotes
    insertVote e@(UpdateVote{..}, _) = HM.alter (append e) uvProposalId
    append e@(UpdateVote{..}, _) Nothing = Just $ HM.singleton uvKey e
    append e@(UpdateVote{..}, _) (Just stVotes) = Just $ HM.insert uvKey e stVotes

-- Old modifier, new modifier
modifyPollModifier :: PollModifier -> PollModifier -> PollModifier
modifyPollModifier pmOld pmNew = PollModifier
    (unionHM pmNewScriptVersions `diff` pmDelScriptVersions pmNew)
    (unionHS pmDelScriptVersions)
    (pmLastAdoptedPV pmNew <|> pmLastAdoptedPV pmOld)
    (unionHM pmNewConfirmed)
    (unionHM pmNewActiveProps `diff` pmDelActiveProps pmNew)
    (unionHS pmDelActiveProps)
    (unionHM pmNewActivePropsIdx `HM.difference` pmDelActivePropsIdx pmNew)
    (unionHM pmDelActivePropsIdx)
  where
    unionHM getter = getter pmNew `HM.union` getter pmOld
    unionHS getter = getter pmNew `HS.union` getter pmOld
    diff a b = a `HM.difference` (HS.toMap b)
