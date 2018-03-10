{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# OPTIONS_GHC -fno-warn-orphans      #-}
module Cardano.Wallet.API.Indices where

import qualified Control.Lens as Lens
import           Universum

import           Cardano.Wallet.API.V1.Types
import           Cardano.Wallet.Util (parseApiUtcTime)
import           Data.String.Conv (toS)
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
    toIndex _ x = utcTimeParser x <|> timePosixParser x
      where
        utcTimeParser t = do
            utcTime <- either (fail . show) pure $ parseApiUtcTime t
            return $ utcTime ^. Lens.from Core.timestampToUTCTimeL
        timePosixParser t =
            view (Lens.from Core.timestampSeconds) <$> readMaybe @Double (toS t)
    accessIx Transaction{..} = let V1 time = txCreationTime in time

-- | A type family mapping a resource 'a' to all its indices.
type family IndicesOf a :: [*] where
    IndicesOf Wallet      = WalletIxs
    IndicesOf Transaction = TransactionIxs
    IndicesOf Account     = AccountIxs

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
