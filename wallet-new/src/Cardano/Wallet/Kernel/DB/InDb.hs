{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Kernel.DB.InDb (
    InDb(..)
  , fromDb
  ) where

import           Universum

import           Control.Lens.TH (makeLenses)
import           Data.SafeCopy (SafeCopy (..), contain, safePut, safeGet)

import qualified Pos.Core as Core
import qualified Pos.Crypto as Core
import qualified Pos.Txp as Core

-- Safecopy instances
import           Pos.Crypto.SafeCopy ()
import           Pos.SafeCopy ()

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
    putCopy (InDb h) = contain $ safePut h
    getCopy = contain $ InDb <$> safeGet

-- TODO: This is really a UTxO again..
instance SafeCopy (InDb (NonEmpty (Core.TxIn, Core.TxOutAux))) where
    putCopy (InDb h) = contain $ safePut h
    getCopy = contain $ InDb <$> safeGet

instance SafeCopy (InDb Core.Address) where
    putCopy (InDb h) = contain $ safePut h
    getCopy = contain $ InDb <$> safeGet

instance SafeCopy (InDb (Core.AddressHash Core.PublicKey)) where
    putCopy (InDb h) = contain $ safePut h
    getCopy = contain $ InDb <$> safeGet

instance SafeCopy (InDb Core.Coin) where
    putCopy (InDb h) = contain $ safePut h
    getCopy = contain $ InDb <$> safeGet

instance SafeCopy (InDb Core.SlotId) where
    putCopy (InDb h) = contain $ safePut h
    getCopy = contain $ InDb <$> safeGet

instance SafeCopy (InDb Core.Timestamp) where
    putCopy (InDb (Core.Timestamp ms)) = contain $ safePut ms
    getCopy = contain $ InDb . Core.Timestamp <$> safeGet

-- TODO this should live with other core type safecopy instances
instance SafeCopy Core.TxAux where
    putCopy (Core.TxAux tx witness)
        = contain $ do safePut tx; safePut witness
    getCopy
        = contain $ Core.TxAux <$> safeGet <*> safeGet

instance SafeCopy (InDb Core.TxAux) where
    putCopy (InDb h) = contain $ safePut h
    getCopy = contain $ InDb <$> safeGet

instance SafeCopy (InDb (Map Core.TxId Core.TxAux)) where
    putCopy (InDb h) = contain $ safePut h
    getCopy = contain $ InDb <$> safeGet

instance SafeCopy (InDb Core.TxId) where
    putCopy (InDb h) = contain $ safePut h
    getCopy = contain $ InDb <$> safeGet

instance SafeCopy (InDb Core.TxIn) where
    putCopy (InDb h) = contain $ safePut h
    getCopy = contain $ InDb <$> safeGet

-- NOTE(adn) This will be provided by @ouroboros as part of work on the BlockMeta
-- storage.
instance SafeCopy (InDb Core.TxId) where
  getCopy = error "TODO: getCopy for (InDb Core.TxId)"
  putCopy = error "TODO: putCopy for (InDb Core.TxId)"

instance SafeCopy (InDb a) => SafeCopy (InDb (Set a)) where
  getCopy = error "TODO: getCopy for (InDb (Set a))"
  putCopy = error "TODO: putCopy for (InDb (Set a))"

instance SafeCopy (InDb (Map Core.TxId Core.SlotId)) where
    putCopy (InDb h) = contain $ safePut h
    getCopy = contain $ InDb <$> safeGet
