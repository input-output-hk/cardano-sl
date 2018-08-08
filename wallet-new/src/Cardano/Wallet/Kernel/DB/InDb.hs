{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Kernel.DB.InDb (
    InDb(..)
  , fromDb
  ) where

import           Universum

import           Control.Lens.TH (makeLenses)
import           Data.SafeCopy (SafeCopy (..))

import qualified Pos.Chain.Block as Core
import qualified Pos.Chain.Txp as Core
import qualified Pos.Core as Core
import qualified Pos.Crypto as Core

{-------------------------------------------------------------------------------
  Wrap core types so that we can make independent serialization decisions
-------------------------------------------------------------------------------}

-- | Wrapped type (with potentially different 'SafeCopy' instance)
newtype InDb a = InDb { _fromDb :: a }
  deriving (Eq, Ord)

instance Functor InDb where
  fmap f = InDb . f . _fromDb

instance Applicative InDb where
  pure = InDb
  InDb f <*> InDb x = InDb (f x)

makeLenses ''InDb

{-------------------------------------------------------------------------------
  Specific SafeCopy instances
-------------------------------------------------------------------------------}

-- TODO @uroboros/ryan [CBR 305] Implement Safecopy instances independently from legacy wallet

instance SafeCopy (InDb Core.Utxo) where
    getCopy = error "TODO: getCopy for (InDb Core.Utxo)"
    putCopy = error "TODO: putCopy for (InDb Core.Utxo)"

-- TODO: This is really a UTxO again..
instance SafeCopy (InDb (NonEmpty (Core.TxIn, Core.TxOutAux))) where
    getCopy = error "TODO: getCopy for (InDb (NonEmpty (Core.TxIn, Core.TxOutAux)))"
    putCopy = error "TODO: putCopy for (InDb (NonEmpty (Core.TxIn, Core.TxOutAux)))"

instance SafeCopy (InDb Core.Address) where
    getCopy = error "TODO: getCopy for (InDb Core.Address)"
    putCopy = error "TODO: putCopy for (InDb Core.Address)"

instance SafeCopy (InDb (Core.AddressHash Core.PublicKey)) where
    getCopy = error "TODO: getCopy for (InDb (Core.AddressHash Core.PublicKey))"
    putCopy = error "TODO: putCopy for (InDb (Core.AddressHash Core.PublicKey))"

instance SafeCopy (InDb Core.Coin) where
    getCopy = error "TODO: getCopy for (InDb Core.Coin)"
    putCopy = error "TODO: putCopy for (InDb Core.Coin)"

instance SafeCopy (InDb Core.SlotId) where
    getCopy = error "TODO: getCopy for (InDb Core.SlotId)"
    putCopy = error "TODO: putCopy for (InDb Core.SlotId)"

instance SafeCopy (InDb Core.BlockHeader) where
    getCopy = error "TODO: getCopy for (InDb Core.BlockHeader)"
    putCopy = error "TODO: putCopy for (InDb Core.BlockHeader)"

instance SafeCopy (InDb Core.HeaderHash) where
    getCopy = error "TODO: getCopy for (InDb Core.HeaderHash)"
    putCopy = error "TODO: putCopy for (InDb Core.HeaderHash)"

instance SafeCopy (InDb Core.Timestamp) where
    getCopy = error "TODO: getCopy for (InDb Core.Timestamp)"
    putCopy = error "TODO: putCopy for (InDb Core.Timestamp)"

-- TODO this should live with other core type safecopy instances
instance SafeCopy Core.TxAux where
    getCopy = error "TODO: getCopy for (InDb Core.TxAux)"
    putCopy = error "TODO: putCopy for (InDb Core.TxAux)"

instance SafeCopy (InDb Core.TxAux) where
    getCopy = error "TODO: getCopy for (InDb Core.TxAux)"
    putCopy = error "TODO: putCopy for (InDb Core.TxAux)"

instance SafeCopy (InDb (Map Core.TxId Core.TxAux)) where
    getCopy = error "TODO: getCopy for (InDb (Map Core.TxId Core.TxAux))"
    putCopy = error "TODO: putCopy for (InDb (Map Core.TxId Core.TxAux))"

instance SafeCopy (InDb Core.TxId) where
    getCopy = error "TODO: getCopy for (InDb Core.TxId)"
    putCopy = error "TODO: putCopy for (InDb Core.TxId)"

instance SafeCopy (InDb Core.TxIn) where
    getCopy = error "TODO: getCopy for (InDb Core.TxIn)"
    putCopy = error "TODO: putCopy for (InDb Core.TxIn)"

instance SafeCopy (InDb a) => SafeCopy (InDb (Set a)) where
  getCopy = error "TODO: getCopy for (InDb (Set a))"
  putCopy = error "TODO: putCopy for (InDb (Set a))"

instance SafeCopy (InDb (Map Core.TxId Core.SlotId)) where
    getCopy = error "TODO: getCopy for (InDb (Map Core.TxId Core.SlotId))"
    putCopy = error "TODO: putCopy for (InDb (Map Core.TxId Core.SlotId))"
