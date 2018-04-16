module Cardano.Wallet.Kernel.DB.InDb (
    InDb(..)
  , fromDb
  ) where

import           Universum

import           Control.Lens.TH (makeLenses)
import           Data.SafeCopy (SafeCopy (..))

import qualified Pos.Core   as Core
import qualified Pos.Crypto as Core
import qualified Pos.Txp    as Core

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

instance SafeCopy (InDb Core.Utxo) where
  getCopy = error "TODO"
  putCopy = error "TODO"

-- TODO: This is really a UTxO again..
instance SafeCopy (InDb (NonEmpty (Core.TxIn, Core.TxOutAux))) where
  getCopy = error "TODO"
  putCopy = error "TODO"

instance SafeCopy (InDb Core.Timestamp) where
  getCopy = error "TODO"
  putCopy = error "TODO"

instance SafeCopy (InDb Core.Address) where
  getCopy = error "TODO"
  putCopy = error "TODO"

instance SafeCopy (InDb (Core.AddressHash Core.PublicKey)) where
  getCopy = error "TODO"
  putCopy = error "TODO"

instance SafeCopy (InDb Core.Coin) where
  getCopy = error "TODO"
  putCopy = error "TODO"

instance SafeCopy (InDb (Map Core.TxId Core.TxAux)) where
  getCopy = error "TODO"
  putCopy = error "TODO"

instance SafeCopy (InDb Core.TxAux) where
  getCopy = error "TODO"
  putCopy = error "TODO"

instance SafeCopy (InDb Core.TxIn) where
  getCopy = error "TODO"
  putCopy = error "TODO"

instance SafeCopy (InDb (Map Core.TxId Core.SlotId)) where
  getCopy = error "TODO"
  putCopy = error "TODO"
