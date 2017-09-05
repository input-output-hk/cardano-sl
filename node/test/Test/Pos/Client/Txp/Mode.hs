{-# LANGUAGE TypeFamilies #-}

-- | Execution mode for tx creation tests.

module Test.Pos.Client.Txp.Mode
       ( TxpTestProperty
       , TxpTestMode
       ) where

import           Universum

import           Control.Monad.Trans.Identity (IdentityT (..))
import qualified Data.ByteString              as BS
import           Test.QuickCheck              (Testable (..), ioProperty)
import           Test.QuickCheck.Monadic      (PropertyM, monadic)

import           Pos.Client.Txp.Addresses     (MonadAddresses (..))
import           Pos.Client.Txp.Util          (TxCreateMode)
import qualified Pos.Constants                as Const
import           Pos.Core                     (HasCoreConstants, makePubKeyAddressBoot)
import           Pos.Crypto                   (deterministicKeyGen)
import           Pos.DB                       (MonadGState (..))

----------------------------------------------------------------------------
-- Mock for TxCreateMode
----------------------------------------------------------------------------

type TxpTestMode = IdentityT IO

instance HasCoreConstants => TxCreateMode TxpTestMode

----------------------------------------------------------------------------
-- Boilerplate TxpTestMode instances
----------------------------------------------------------------------------

instance MonadGState TxpTestMode where
    gsAdoptedBVData = pure Const.genesisBlockVersionData

instance MonadAddresses TxpTestMode where
    type AddrData TxpTestMode = ()
    getNewAddress _ = pure address
      where
        -- seed for address generation is a ByteString with 32 255's
        seedSize = 32
        seed = BS.replicate seedSize (255 :: Word8)
        address = makePubKeyAddressBoot $ fst $ deterministicKeyGen seed

----------------------------------------------------------------------------
-- Property
----------------------------------------------------------------------------

type TxpTestProperty = PropertyM TxpTestMode

-- Cannot write a general OVERLAPPABLE instance with MonadTrans since
-- type families cannot be OVERLAPPABLE.
instance MonadAddresses TxpTestProperty where
    type AddrData TxpTestProperty = AddrData TxpTestMode
    getNewAddress = lift . getNewAddress

instance HasCoreConstants => Testable (TxpTestProperty a) where
    property = monadic (ioProperty . runIdentityT)
