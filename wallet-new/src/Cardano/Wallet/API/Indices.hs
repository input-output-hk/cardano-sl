{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
module Cardano.Wallet.API.Indices where

import           Universum

import           Cardano.Wallet.API.V1.Types
import           Data.String.Conv (toS)
import qualified Pos.Core as Core

import           Data.IxSet.Typed (Indexable (..), IsIndexOf, IxSet, ixFun, ixList)

class ToIndex a ix where
    -- | How to build this index from the input 'Text'.
    toIndex :: Proxy a -> Text -> Maybe ix

instance ToIndex Wallet WalletId where
    toIndex _ x = Just (WalletId x)

instance ToIndex Wallet Core.Coin where
    -- TODO: Temporary.
    toIndex _ x = Core.mkCoin <$> readMaybe (toS x)

-- | A type family mapping a resource 'a' to all its indices.
type family IndicesOf a :: [*] where
    IndicesOf Wallet      = WalletIxs
    IndicesOf Transaction = TransactionIxs

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
type WalletIxs = '[WalletId, Core.Coin]
type TransactionIxs = '[TxId]

instance Indexable WalletIxs Wallet where
  indices = ixList (ixFun (\Wallet{..} -> [walId]))
                   (ixFun (\Wallet{..} -> [walBalance]))

instance Indexable TransactionIxs Transaction where
  indices = ixList (ixFun (\Transaction{..} -> [txId]))
