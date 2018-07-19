{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}

module Cardano.Wallet.Kernel.CoinSelection.Generic.Grouped (
    -- * Domain
    Grouped(..)
  , Sum(..)
  , Size(..)
    -- * Conditions for grouping
  , CanGroup(..)
  , unsafeUtxoLookup
    -- * Grouped UTxO
  , GroupedUtxo(..)
  , groupUtxo
  ) where

import           Universum hiding (Sum (..), group)

import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Formatting (bprint, build, sformat, (%))
import qualified Formatting.Buildable
import           Serokell.Util (listJson)

import           Cardano.Wallet.Kernel.CoinSelection.Generic

{-------------------------------------------------------------------------------
  Domain
-------------------------------------------------------------------------------}

-- | Group of inputs or outputs
newtype Grouped a = Group { getGroup :: [a] }
  deriving (Eq, Ord)

-- | The summarized value of a group of outputs
newtype Sum a = Sum { getSum :: a }
  deriving (Show, Eq, Ord, Buildable, IsValue)

-- | A grouped domain is useful when we cannot pick single entries from the
-- UTxO, but instead must--for cryptographic reasons--sometimes bundle inputs
-- (if we use one, we use all of them). Grouping the domain means that the
-- coin selection algorithm itself doesn't need to be aware of this.
instance CoinSelDom dom => CoinSelDom (Grouped dom) where
  type    Input     (Grouped dom) = Grouped   (Input     dom)
  type    Output    (Grouped dom) = Grouped   (Output    dom)
  type    UtxoEntry (Grouped dom) = Grouped   (UtxoEntry dom)
  type    Value     (Grouped dom) = Sum       (Value     dom)
  newtype Size      (Grouped dom) = GroupSize { groupSize :: Size dom }

  -- The value of a group of outputs is their sum
  outVal = Sum . unsafeValueSum . map outVal . getGroup

  -- We cannot subtract more than the value of the group, which is its sum;
  -- hence, subtracting the fee proportionally over each individual output
  -- can fail only if the total fee is larger than the sum.
  -- (Ignoring rounding up errors.)
  outSubFee (Fee (Sum fee)) = fmap Group
                            . mapM (uncurry outSubFee)
                            . divvyFee outVal (Fee fee)
                            . getGroup

  utxoEntryInp = Group . map utxoEntryInp . getGroup
  utxoEntryOut = Group . map utxoEntryOut . getGroup

  sizeZero   = GroupSize sizeZero
  sizeIncr   = \(Group es) (GroupSize s) ->
                  GroupSize $ foldl' (flip sizeIncr) s es
  sizeToWord = sizeToWord . groupSize

{-------------------------------------------------------------------------------
  Conditions under which we can group UTxOs
-------------------------------------------------------------------------------}

class (PickFromUtxo utxo, HasAddress (Dom utxo)) => CanGroup utxo where
  -- | Lookup a specific entry
  --
  -- Used for grouping.
  utxoLookup :: Input (Dom utxo) -> utxo -> Maybe (UtxoEntry (Dom utxo))

  -- | Remove entries from the UTxO
  --
  -- Used for grouping
  utxoRemoveInputs :: Set (Input (Dom utxo)) -> utxo -> utxo

  -- Default implementations

  default utxoLookup :: StandardUtxo utxo
                     => Input (Dom utxo) -> utxo -> Maybe (UtxoEntry (Dom utxo))
  utxoLookup i = fmap (i, ) . Map.lookup i . coerce

  default utxoRemoveInputs :: StandardUtxo utxo
                           => Set (Input (Dom utxo)) -> utxo -> utxo
  utxoRemoveInputs inps = coerce . removeInputs . coerce
    where
      removeInputs :: Map (Input (Dom utxo)) (Output (Dom utxo))
                   -> Map (Input (Dom utxo)) (Output (Dom utxo))
      removeInputs = (`withoutKeys` inps)

-- | UTxO lookup when the input is guaranteed to exist
unsafeUtxoLookup :: CanGroup utxo
                 => Input (Dom utxo) -> utxo -> UtxoEntry (Dom utxo)
unsafeUtxoLookup inp utxo =
    fromMaybe (error err) $ utxoLookup inp utxo
  where
    err = sformat ("unsafeUtxoLookup: unknown input " % build) inp

{-------------------------------------------------------------------------------
  Grouped UTxO
-------------------------------------------------------------------------------}

-- | Group in a grouped UTxO
--
-- NOTE: We don't record this as a set of UTxO entries, so that when we deal
-- with grouped UTxOs, we don't accidentally use UTxO entries more than once
-- (we don't modify `groupsByAddr` as we pick entries).
--
-- See also 'GroupedUtxo'
type Group utxo = Set (Input (Dom utxo))

-- | In a grouped UTxO, entries in a group can only be spent all at once
-- or not at all.
data GroupedUtxo utxo = GroupedUtxo {
      -- | The underlying UTxO
      underlyingUtxo :: utxo

      -- | Groups by addresss
      --
      -- Should have the property that for all addresses @a@, if
      --
      -- > groupsByAddr Map.! a == group
      --
      -- then for all @a'@ in @group@,
      --
      -- > groupsByAddr Map.! a' == group
    , groupsByAddr   :: Map (Address (Dom utxo)) (Group utxo)
    }

-- | Default translation from UTxOs represented as maps to grouped UTxOs
--
-- The cost of this translation is @O(n log m)@, with @n@ the length of the
-- UTxO and @m@ the size of the largest group. In typical cases the size of
-- groups will be bound (and small), so that this is linear in the size of
-- the UTxO.
groupUtxo :: forall utxo. (StandardUtxo utxo, HasAddress (Dom utxo))
          => utxo -> GroupedUtxo utxo
groupUtxo utxo = GroupedUtxo utxo groups
  where
    groups :: Map (Address (Dom utxo)) (Group utxo)
    groups = foldl' insert Map.empty (Map.toList (coerce utxo))

    insert :: Map (Address (Dom utxo)) (Group utxo)
           -> (Input (Dom utxo), Output (Dom utxo))
           -> Map (Address (Dom utxo)) (Group utxo)
    insert acc (i, o) = Map.alter f (outAddr o) acc
      where
        f :: Maybe (Group utxo) -> Maybe (Group utxo)
        f Nothing      = Just $ Set.singleton i
        f (Just group) = Just $ Set.insert i group

instance CanGroup utxo => PickFromUtxo (GroupedUtxo utxo) where
  type Dom (GroupedUtxo utxo) = Grouped (Dom utxo)

  pickRandom :: MonadRandom m
             => GroupedUtxo utxo
             -> m (Maybe (Grouped (UtxoEntry (Dom utxo)), GroupedUtxo utxo))
  pickRandom groupedUtxo@GroupedUtxo{..} =
      fmap mkGroup <$> pickRandom underlyingUtxo
    where
      mkGroup :: (UtxoEntry (Dom utxo), utxo)
              -> (UtxoEntry (Dom (GroupedUtxo utxo)), GroupedUtxo utxo)
      mkGroup (io, utxo) = (
            resolved
          , GroupedUtxo (utxoRemoveInputs group utxo) groupsByAddr
          )
        where
          (group, resolved) = resolveGroup groupedUtxo io

  pickLargest :: Word64
              -> GroupedUtxo utxo
              -> [(Grouped (UtxoEntry (Dom utxo)), GroupedUtxo utxo)]
  pickLargest n groupedUtxo@GroupedUtxo{..} =
      mkGroups Set.empty $ pickLargest n underlyingUtxo
    where
      mkGroups :: Set (Input (Dom utxo)) -- All elements returned so far
               -> [(UtxoEntry (Dom utxo), utxo)]
               -> [(UtxoEntry (Dom (GroupedUtxo utxo)), GroupedUtxo utxo)]
      mkGroups _ [] = []
      mkGroups returnedAlready ((io, utxo):entries)
            -- An element of a group can only already be returned if and only
            -- if /all/ elements of that group have been returned.
            -- This means that we can just skip this entry.
          | utxoEntryInp io `Set.member` returnedAlready =
              mkGroups returnedAlready entries
            -- Adding this new group would push us over the limit.
            -- We terminate (possibly early with less than @n@).
          | Set.size returned > fromIntegral n =
              []
            -- By the same argument, now /none/ of the elements in the group
            -- have yet been returned.
          | otherwise =
                ( resolved
                , GroupedUtxo (utxoRemoveInputs returned utxo) groupsByAddr
                )
              : mkGroups returned entries
        where
          (group, resolved) = resolveGroup groupedUtxo io
          returned = Set.union returnedAlready group

  utxoBalance :: GroupedUtxo utxo -> Sum (Value (Dom utxo))
  utxoBalance = Sum . utxoBalance . underlyingUtxo

-- | Resolve a group
--
-- Returns the group as the original set of inputs, along with the resolved UTxO
-- entries.
resolveGroup :: forall utxo. CanGroup utxo
             => GroupedUtxo utxo
             -> UtxoEntry (Dom utxo)
             -> (Group utxo, UtxoEntry (Dom (GroupedUtxo utxo)))
resolveGroup GroupedUtxo{..} io =
    (group, Group resolved)
  where
    group :: Group utxo
    group = Map.findWithDefault
             (Set.singleton (utxoEntryInp io))
             (utxoEntryAddr io)
             groupsByAddr

    resolved :: [UtxoEntry (Dom utxo)]
    resolved = map (`unsafeUtxoLookup` underlyingUtxo) $ Set.toList group

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable a => Buildable (Grouped a) where
  build (Group as) = bprint listJson as
