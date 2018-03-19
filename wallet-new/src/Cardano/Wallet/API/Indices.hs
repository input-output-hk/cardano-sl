{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}

{-# OPTIONS_GHC -fno-warn-orphans      #-}

module Cardano.Wallet.API.Indices where

import           Universum

import           Cardano.Wallet.API.V1.Types
import           Data.String.Conv (toS)
import           GHC.TypeLits
import qualified Pos.Core as Core
import           Pos.Crypto (decodeHash)

import           Data.IxSet.Typed (Indexable (..), IsIndexOf, IxSet, ixFun, ixList)

-- | 'ToIndex' represents the witness that we can build an index 'ix' for a resource 'a'
-- from an input 'Text'.
class ToIndex a ix where
    -- | How to build this index from the input 'Text'.
    toIndex  :: Proxy a -> Text -> Maybe ix
    -- | How to access this index from the input data.
    accessIx :: (a -> ix)

instance ToIndex Wallet WalletId where
    toIndex _ x = Just (WalletId x)
    accessIx Wallet{..} = walId

instance ToIndex Wallet Core.Coin where
    toIndex _ x = case readMaybe (toS x) of
        Nothing -> Nothing
        Just c  | c > Core.maxCoinVal -> Nothing
        Just c  -> Just (Core.mkCoin c)
    accessIx Wallet{..} = let (V1 balance) = walBalance in balance

instance ToIndex Transaction Core.TxId where
    toIndex _ = rightToMaybe . decodeHash
    accessIx Transaction{..} = let V1 ti = txId in ti

instance ToIndex Transaction Core.Timestamp where
    toIndex _ = Core.parseTimestamp
    accessIx Transaction{..} = let V1 time = txCreationTime in time

-- | A type family mapping a resource 'a' to all its indices.
type family IndicesOf a :: [*] where
    IndicesOf Wallet      = WalletIxs
    IndicesOf Transaction = TransactionIxs
    IndicesOf Account     = AccountIxs

-- | This type family allows you to recover the query parameter if you know
-- the resource and index into that resource.
type family IndexToQueryParam resource ix where
    IndexToQueryParam Wallet  Core.Coin    = "balance"
    IndexToQueryParam Wallet  WalletId     = "id"
    IndexToQueryParam Account AccountIndex = "id"

    -- | This is the fallback case. It will trigger a type error if you use
    -- 'IndexToQueryParam' with a pairing that is invalid. We want this to
    -- trigger early, so that we don't get Weird Errors later on with stuck
    -- types.
    IndexToQueryParam res ix = TypeError (
        'Text "You used `IndextoQueryParam' with the following resource:"
        ':$$: 'Text "    " ':<>: 'ShowType res
        ':$$: 'Text "and index type:"
        ':$$: 'Text "    " ':<>: 'ShowType ix
        ':$$: 'Text "But no instance for that type was defined."
        ':$$: 'Text "Perhaps you mismatched a resource and an index?"
        ':$$: 'Text "Or, maybe you need to add a type instance to `IndexToQueryParam'."
        )

-- | A variant of an 'IxSet' where the indexes are determined statically by the resource type.
type IxSet' a        = IxSet (IndicesOf a) a

-- | A variant of the 'Indexable' constraint where the indexes are determined statically by the resource type.
type Indexable' a    = Indexable (IndicesOf a) a

-- | A variant of the 'IsIndexOf' constraint where the indexes are determined statically by the resource type.
type IsIndexOf' a ix = IsIndexOf ix (IndicesOf a)

--
-- Indices for all the major resources
--

-- | The indices for each major resource.
type WalletIxs      = '[WalletId, Core.Coin]
type TransactionIxs = '[Core.TxId, Core.Timestamp]
type AccountIxs     = '[AccountIndex]

instance Indexable WalletIxs Wallet where
  indices = ixList (ixFun (\Wallet{..} -> [walId]))
                   (ixFun (\Wallet{..} -> let (V1 balance) = walBalance in [balance]))

instance Indexable TransactionIxs Transaction where
  indices = ixList (ixFun (\Transaction{..} -> let (V1 tid) = txId in [tid]))
                   (ixFun (\Transaction{..} -> let (V1 time) = txCreationTime in [time]))

instance Indexable AccountIxs Account where
  indices = ixList (ixFun (\Account{..} -> [accIndex]))
