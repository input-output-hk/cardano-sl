-- | Functions working with PollModifier.

module Pos.Update.Poll.Modifier
       ( modifyPollModifier
       ) where

import           Data.Default          (Default (def))
import qualified Data.HashMap.Strict   as HM
import           Universum

import           Pos.Update.Poll.Types (PollModifier (..))

instance Default PollModifier where
    def =
        PollModifier
        { pmBVs = mempty
        , pmAdoptedBVFull = Nothing
        , pmConfirmed = mempty
        , pmConfirmedProps = mempty
        , pmActiveProps = mempty
        , pmDelActivePropsIdx = mempty
        , pmSlottingData = Nothing
        , pmEpochProposers = mempty
        }

-- | Unite two PollModifiers. Second argument dominates, i. e. if
-- there are two confliciting modifications, the second one wins.
modifyPollModifier :: PollModifier -> PollModifier -> PollModifier
modifyPollModifier pmOld pmNew = PollModifier
    (pmBVs pmOld <> pmBVs pmNew)
    (pmAdoptedBVFull pmNew <|> pmAdoptedBVFull pmOld)
    (pmConfirmed pmOld <> pmConfirmed pmNew)
    (pmConfirmedProps pmOld <> pmConfirmedProps pmNew)
    (pmActiveProps pmOld <> pmActiveProps pmNew)
    (unionHM pmDelActivePropsIdx)
    (pmSlottingData pmNew <|> pmSlottingData pmOld)
    (pmEpochProposers pmNew)
  where
    unionHM :: (Hashable k, Eq k) => (PollModifier -> HashMap k v) -> HashMap k v
    unionHM getter = getter pmNew `HM.union` getter pmOld

instance Monoid PollModifier where
    mempty = def
    mappend = modifyPollModifier

instance Semigroup PollModifier
