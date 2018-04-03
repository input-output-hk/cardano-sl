{-# LANGUAGE TypeFamilies #-}

-- | Execution mode for tx creation tests.

module Test.Pos.Client.Txp.Mode
       ( TxpTestProperty
       , TxpTestMode
       , HasTxpConfigurations
       , withBVData
       ) where

import           Universum

import qualified Data.ByteString as BS
import           Test.QuickCheck (Testable (..), ioProperty)
import           Test.QuickCheck.Monadic (PropertyM, monadic)

import           Pos.Client.Txp.Addresses (MonadAddresses (..))
import           Pos.Configuration (HasNodeConfiguration)
import           Pos.Core (Address, BlockVersionData, HasConfiguration, makePubKeyAddressBoot)
import           Pos.Core.Configuration (HasGenesisBlockVersionData, genesisBlockVersionData)
import           Pos.Crypto (deterministicKeyGen)
import           Pos.DB (MonadGState (..))
import           Pos.Ssc.Configuration (HasSscConfiguration)
import           Pos.Update.Configuration (HasUpdateConfiguration)

----------------------------------------------------------------------------
-- Configuration propagation
----------------------------------------------------------------------------

type HasTxpConfigurations =
       ( HasNodeConfiguration
       , HasSscConfiguration
       , HasConfiguration
       , HasUpdateConfiguration
       , HasGenesisBlockVersionData
       )

----------------------------------------------------------------------------
-- Mock for TxCreateMode
----------------------------------------------------------------------------

type TxpTestMode = ReaderT BlockVersionData IO

----------------------------------------------------------------------------
-- Boilerplate TxpTestMode instances
----------------------------------------------------------------------------

instance HasTxpConfigurations => MonadGState TxpTestMode where
    gsAdoptedBVData = ask

instance HasTxpConfigurations => MonadAddresses TxpTestMode where
    type AddrData TxpTestMode = ()
    getNewAddress _ = pure fakeAddressForMonadAddresses
    getFakeChangeAddress = pure fakeAddressForMonadAddresses

fakeAddressForMonadAddresses :: Address
fakeAddressForMonadAddresses = address
  where
    -- seed for address generation is a ByteString with 32 255's
    seedSize = 32
    seed = BS.replicate seedSize (255 :: Word8)
    address = makePubKeyAddressBoot $ fst $ deterministicKeyGen seed

withBVData
  :: MonadReader BlockVersionData m
  => BlockVersionData
  -> m a
  -> m a
withBVData bvd = local (const bvd)

----------------------------------------------------------------------------
-- Property
----------------------------------------------------------------------------

type TxpTestProperty = PropertyM TxpTestMode

-- Cannot write a general OVERLAPPABLE instance with MonadTrans since
-- type families cannot be OVERLAPPABLE.
instance HasTxpConfigurations => MonadAddresses TxpTestProperty where
    type AddrData TxpTestProperty = AddrData TxpTestMode
    getNewAddress = lift . getNewAddress
    getFakeChangeAddress = lift getFakeChangeAddress

instance (HasTxpConfigurations, Testable a) => Testable (TxpTestProperty a) where
    property = monadic (ioProperty . flip runReaderT genesisBlockVersionData)
