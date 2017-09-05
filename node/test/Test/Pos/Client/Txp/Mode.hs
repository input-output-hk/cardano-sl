{-# LANGUAGE TypeFamilies #-}

-- | Execution mode for tx creation tests.

module Test.Pos.Client.Txp.Mode
       ( TxpTestProperty
       , TxpTestMode
       ) where

import           Universum

import           Control.Lens             (makeClassy, makeLensesWith)
import qualified Data.ByteString          as BS
import           Test.QuickCheck          (Arbitrary (..), Gen, Property, Testable (..),
                                           forAll, ioProperty)
import           Test.QuickCheck.Monadic  (PropertyM, monadic)

import           Pos.Client.Txp.Addresses (MonadAddresses (..))
import           Pos.Client.Txp.Util      (TxCreateMode)
import qualified Pos.Constants            as Const
import           Pos.Core                 (HasCoreConstants)
import           Pos.DB                   (MonadGState (..))
import           Pos.Util.Util            (postfixLFields)
import           Test.Pos.Client.Txp.Util (generateAddressWithKey, seedSize)

----------------------------------------------------------------------------
-- TestParams
----------------------------------------------------------------------------

-- An object of this type has instance Arbitrary and is used as the source
-- of randomness in the tests.
data TxpTestParams = TxpTestParams
    { _ttpDummy :: !()
    } deriving (Show)

makeClassy ''TxpTestParams

instance Arbitrary TxpTestParams where
    arbitrary = do
        let _ttpDummy = ()
        pure TxpTestParams {..}

----------------------------------------------------------------------------
-- Mock for TxCreateMode
----------------------------------------------------------------------------

type TxpTestMode = ReaderT TxpTestContext IO

data TxpTestContext = TxpTestContext
    { ttcParams :: !TxpTestParams }

makeLensesWith postfixLFields ''TxpTestContext

instance HasCoreConstants => TxCreateMode TxpTestMode

----------------------------------------------------------------------------
-- Mock initialization
----------------------------------------------------------------------------

initTxpTestContext
    :: (HasCoreConstants, MonadIO m)
    => TxpTestParams
    -> m TxpTestContext
initTxpTestContext tp@TxpTestParams {..} = do
    let ttcParams = tp
    pure TxpTestContext {..}

runTxpTestMode
    :: HasCoreConstants
    => TxpTestParams
    -> TxpTestMode a
    -> IO a
runTxpTestMode tp action = do
    ctx <- initTxpTestContext tp
    runReaderT action ctx

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
        seed = BS.replicate seedSize (255 :: Word8)
        (_, address) = generateAddressWithKey seed

----------------------------------------------------------------------------
-- Property
----------------------------------------------------------------------------

type TxpTestProperty = PropertyM TxpTestMode

-- Cannot write a general OVERLAPPABLE instance with MonadTrans since
-- type families cannot be OVERLAPPABLE.
instance MonadAddresses TxpTestProperty where
    type AddrData TxpTestProperty = AddrData TxpTestMode
    getNewAddress = lift . getNewAddress

txCreatePropertyToProperty
    :: HasCoreConstants
    => Gen TxpTestParams
    -> TxpTestProperty a
    -> Property
txCreatePropertyToProperty tpGen txpTestProperty =
    forAll tpGen $ \tp ->
        monadic (ioProperty . runTxpTestMode tp) txpTestProperty

instance HasCoreConstants => Testable (TxpTestProperty a) where
    property = txCreatePropertyToProperty arbitrary
