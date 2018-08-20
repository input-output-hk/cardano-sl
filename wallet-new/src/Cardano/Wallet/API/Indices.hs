{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}

{-# OPTIONS_GHC -fno-warn-orphans      #-}

module Cardano.Wallet.API.Indices (
    module Cardano.Wallet.API.Indices
    -- * Re-exports from IxSet for convenience
    -- (these were previously /defined/ in this module)
  , IndicesOf
  , IxSet
  , Indexable
  , IsIndexOf
  ) where

import           Universum

import           Cardano.Wallet.API.V1.Types
import qualified Data.Text as T
import           GHC.TypeLits
import qualified Pos.Core as Core
import qualified Pos.Core.Txp as Txp
import           Pos.Crypto (decodeHash)

import           Cardano.Wallet.Kernel.DB.Util.IxSet (HasPrimKey (..),
                     Indexable, IndicesOf, IsIndexOf, IxSet, OrdByPrimKey,
                     ixFun, ixList)
import qualified Data.IxSet.Typed as IxSet

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

--
-- Primary and secondary indices for V1 types
--

instance HasPrimKey Wallet where
    type PrimKey Wallet = WalletId
    primKey = walId

instance HasPrimKey Account where
    type PrimKey Account = AccountIndex
    primKey = accIndex

instance HasPrimKey Transaction where
    type PrimKey Transaction = V1 Txp.TxId
    primKey = txId

instance HasPrimKey WalletAddress where
    type PrimKey WalletAddress = V1 Core.Address
    primKey = addrId

-- | The secondary indices for each major resource.
type SecondaryWalletIxs        = '[Core.Coin, V1 Core.Timestamp]
type SecondaryTransactionIxs   = '[V1 Core.Timestamp]
type SecondaryAccountIxs       = '[]
type SecondaryWalletAddressIxs = '[]

type instance IndicesOf Wallet        = SecondaryWalletIxs
type instance IndicesOf Account       = SecondaryAccountIxs
type instance IndicesOf Transaction   = SecondaryTransactionIxs
type instance IndicesOf WalletAddress = SecondaryWalletAddressIxs

--
-- Indexable instances for V1 types
--
-- TODO [CBR-356] These should not exist! We should not create 'IxSet's
-- (with their indices) on the fly. Fortunately, the only one for which this
-- is /really/ important is addresses, for which we already have special
-- cases. Nonetheless, the instances below should also go.
--
-- Instance for 'WalletAddress' is available only in
-- "Cardano.Wallet.API.V1.LegacyHandlers.Instances". The same should be done
-- for the other instances here.
--

instance IxSet.Indexable (WalletId ': SecondaryWalletIxs)
                         (OrdByPrimKey Wallet) where
    indices = ixList (ixFun ((:[]) . unV1 . walBalance))
                     (ixFun ((:[]) . walCreatedAt))

instance IxSet.Indexable (V1 Txp.TxId ': SecondaryTransactionIxs)
                         (OrdByPrimKey Transaction) where
    indices = ixList (ixFun (\Transaction{..} -> [txCreationTime]))

instance IxSet.Indexable (AccountIndex ': SecondaryAccountIxs)
                         (OrdByPrimKey Account) where
    indices = ixList

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
        'Text "You used `IndexToQueryParam' with the following resource:"
        ':$$: 'Text "    " ':<>: 'ShowType res
        ':$$: 'Text "and index type:"
        ':$$: 'Text "    " ':<>: 'ShowType ix
        ':$$: 'Text "But no instance for that type was defined."
        ':$$: 'Text "Perhaps you mismatched a resource and an index?"
        ':$$: 'Text "Or, maybe you need to add a type instance to `IndexToQueryParam'."
        )

-- | Type-level composition of 'KnownSymbol' and 'IndexToQueryParam'
--
-- TODO: Alternatively, it would be possible to get rid of 'IndexToQueryParam'
-- completely and just have the 'KnownQueryParam' class.
class KnownSymbol (IndexToQueryParam resource ix) => KnownQueryParam resource ix

instance KnownQueryParam Account AccountIndex
instance KnownQueryParam Wallet Core.Coin
instance KnownQueryParam Wallet WalletId
instance KnownQueryParam Wallet (V1 Core.Timestamp)
instance KnownQueryParam WalletAddress (V1 Core.Address)
instance KnownQueryParam Transaction (V1 Txp.TxId)
instance KnownQueryParam Transaction (V1 Core.Timestamp)
