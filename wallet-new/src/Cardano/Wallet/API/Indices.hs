{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}

{-# OPTIONS_GHC -fno-warn-orphans      #-}

module Cardano.Wallet.API.Indices where

import           Universum

import           Cardano.Wallet.API.V1.Types
import qualified Data.Text as T
import           GHC.TypeLits
import qualified Pos.Core as Core
import qualified Pos.Core.Txp as Txp
import           Pos.Crypto (decodeHash)

import           Data.IxSet.Typed (Indexable (..), IsIndexOf, IxSet, ixFun,
                     ixList)

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
    toIndex _ x = case readMaybe (T.unpack x) of
        Nothing                       -> Nothing
        Just c  | c > Core.maxCoinVal -> Nothing
        Just c                        -> Just (Core.mkCoin c)
    accessIx Wallet{..} = let (V1 balance) = walBalance in balance

instance ToIndex Wallet (V1 Core.Timestamp) where
    toIndex _ = fmap V1 . Core.parseTimestamp
    accessIx = walCreatedAt

instance ToIndex Transaction (V1 Txp.TxId) where
    toIndex _ = fmap V1 . rightToMaybe . decodeHash
    accessIx Transaction{..} = txId

instance ToIndex Transaction (V1 Core.Timestamp) where
    toIndex _ = fmap V1 . Core.parseTimestamp
    accessIx Transaction{..} = txCreationTime

instance ToIndex WalletAddress (V1 Core.Address) where
    toIndex _ = fmap V1 . either (const Nothing) Just . Core.decodeTextAddress
    accessIx WalletAddress{..} = addrId

-- | A type family mapping a resource 'a' to all its indices.
type family IndicesOf a :: [*] where
    IndicesOf Wallet        = WalletIxs
    IndicesOf Transaction   = TransactionIxs
    IndicesOf Account       = AccountIxs
    IndicesOf WalletAddress = WalletAddressIxs

-- | A variant of an 'IxSet' where the indexes are determined statically by the resource type.
type IxSet' a        = IxSet (IndicesOf a) a

-- | A variant of the 'Indexable' constraint where the indexes are determined statically by the resource type.
type Indexable' a    = Indexable (IndicesOf a) a

-- | A variant of the 'IsIndexOf' constraint where the indexes are determined statically by the resource type.
type IsIndexOf' a ix = IsIndexOf ix (IndicesOf a)

-- | This constraint expresses that @ix@ is a valid index of @a@.
type IndexRelation a ix =
    ( Indexable' a
    , IsIndexOf' a ix
    , ToIndex a ix
    , Typeable ix
    , KnownSymbol (IndexToQueryParam a ix)
    )

--
-- Indices for all the major resources
--

-- | The indices for each major resource.
type WalletIxs        = '[WalletId, Core.Coin, V1 Core.Timestamp]
type TransactionIxs   = '[V1 Txp.TxId, V1 Core.Timestamp]
type AccountIxs       = '[AccountIndex]
type WalletAddressIxs = '[V1 Core.Address]

instance Indexable WalletIxs Wallet where
  indices = ixList (ixFun (\Wallet{..} -> [walId]))
                   (ixFun (\Wallet{..} -> let (V1 balance) = walBalance in [balance]))
                   (ixFun (\Wallet{..} -> [walCreatedAt]))

instance Indexable TransactionIxs Transaction where
  indices = ixList (ixFun (\Transaction{..} -> [txId]))
                   (ixFun (\Transaction{..} -> [txCreationTime]))

instance Indexable AccountIxs Account where
  indices = ixList (ixFun (\Account{..} -> [accIndex]))

instance Indexable WalletAddressIxs WalletAddress where
  indices = ixList (ixFun (\WalletAddress{..} -> [addrId]))

-- | Extract the parameter names from a type leve list with the shape
type family ParamNames res xs where
    ParamNames res '[] =
        '[]
    ParamNames res (ty ': xs) =
        IndexToQueryParam res ty ': ParamNames res xs

-- | This type family allows you to recover the query parameter if you know
-- the resource and index into that resource.
type family IndexToQueryParam resource ix where
    IndexToQueryParam Account AccountIndex            = "id"

    IndexToQueryParam Wallet  Core.Coin               = "balance"
    IndexToQueryParam Wallet  WalletId                = "id"
    IndexToQueryParam Wallet  (V1 Core.Timestamp)     = "created_at"

    IndexToQueryParam WalletAddress (V1 Core.Address) = "address"

    IndexToQueryParam Transaction (V1 Txp.TxId)      = "id"
    IndexToQueryParam Transaction (V1 Core.Timestamp) = "created_at"

    -- This is the fallback case. It will trigger a type error if you use
    -- 'IndexToQueryParam'' with a pairing that is invalid. We want this to
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
