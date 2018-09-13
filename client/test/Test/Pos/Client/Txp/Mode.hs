{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Execution mode for tx creation tests.

module Test.Pos.Client.Txp.Mode
       ( TxpTestProperty
       , TxpTestMode
       , withBVData
       ) where

import           Universum

import qualified Data.ByteString as BS
import           Test.QuickCheck (Testable (..), ioProperty)
import           Test.QuickCheck.Monadic (PropertyM, monadic)

import           Pos.Client.Txp.Addresses (MonadAddresses (..))
import           Pos.Core (Address, makePubKeyAddressBoot)
import           Pos.Core.Update (BlockVersionData)
import           Pos.Crypto (deterministicKeyGen)
import           Pos.DB (MonadGState (..))

import           Test.Pos.Core.Dummy (dummyBlockVersionData)

----------------------------------------------------------------------------
-- Mock for TxCreateMode
----------------------------------------------------------------------------

type TxpTestMode = ReaderT BlockVersionData IO

----------------------------------------------------------------------------
-- Boilerplate TxpTestMode instances
----------------------------------------------------------------------------

instance MonadGState TxpTestMode where
    gsAdoptedBVData = ask

instance MonadAddresses TxpTestMode where
    type AddrData TxpTestMode = ()
    getNewAddress _ _ = pure fakeAddressForMonadAddresses
    getFakeChangeAddress _ = pure fakeAddressForMonadAddresses

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
instance MonadAddresses TxpTestProperty where
    type AddrData TxpTestProperty = AddrData TxpTestMode
    getNewAddress epochSlots = lift . getNewAddress epochSlots
    getFakeChangeAddress = lift . getFakeChangeAddress

instance Testable a => Testable (TxpTestProperty a) where
    property = monadic (ioProperty . flip runReaderT dummyBlockVersionData)
