-- | Functions working with PollModifier.

module Pos.Update.Poll.Modifier
       ( modifyPollModifier
       ) where

import           Data.Default          (Default (def))
import qualified Data.HashMap.Strict   as HM
import qualified Data.HashSet          as HS
import           Universum

import           Pos.Update.Poll.Types (PollModifier (..))

instance Default PollModifier where
    def =
        PollModifier
        { pmNewBVs = mempty
        , pmDelBVs = mempty
        , pmLastAdoptedBV = Nothing
        , pmNewConfirmed = mempty
        , pmDelConfirmed = mempty
        , pmNewActiveProps = mempty
        , pmNewConfirmedProps = mempty
        , pmDelConfirmedProps = mempty
        , pmDelActiveProps = mempty
        , pmNewActivePropsIdx = mempty
        , pmDelActivePropsIdx = mempty
        }

-- | Unite two PollModifiers. Second argument dominates, i. e. if
-- there are two confliciting modifications, the second one wins.
modifyPollModifier :: PollModifier -> PollModifier -> PollModifier
modifyPollModifier pmOld pmNew = PollModifier
    (unionHM pmNewBVs `diffMapSet` pmDelBVs pmNew)
    (unionHS pmDelBVs)
    (pmLastAdoptedBV pmNew <|> pmLastAdoptedBV pmOld)
    (unionHM pmNewConfirmed `diffMapSet` pmDelConfirmed pmNew)
    (unionHS pmDelConfirmed)
    (unionHM pmNewConfirmedProps `diffMapSet` pmDelConfirmedProps pmNew)
    (unionHS pmDelConfirmedProps)
    (unionHM pmNewActiveProps `diffMapSet` pmDelActiveProps pmNew)
    (unionHS pmDelActiveProps)
    (unionHM pmNewActivePropsIdx `HM.difference` pmDelActivePropsIdx pmNew)
    (unionHM pmDelActivePropsIdx)
  where
    unionHM :: (Hashable k, Eq k) => (PollModifier -> HashMap k v) -> HashMap k v
    unionHM getter = getter pmNew `HM.union` getter pmOld
    unionHS :: (Hashable a, Eq a) => (PollModifier -> HashSet a) -> HashSet a
    unionHS getter = getter pmNew `HS.union` getter pmOld
    diffMapSet :: (Hashable k, Eq k) => HashMap k v -> HashSet k -> HashMap k v
    diffMapSet a b = a `HM.difference` (HS.toMap b)

instance Monoid PollModifier where
    mempty = def
    mappend = modifyPollModifier

instance Semigroup PollModifier
