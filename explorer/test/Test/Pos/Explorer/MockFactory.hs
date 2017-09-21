
-- | Factory to create mock objects
-- | It might be merged with other factories or test utilities of others modules
-- | such as `Test.Pos.Client.Txp.UtilSpec`.

module Test.Pos.Explorer.MockFactory
       ( mkTxOut
       , secretKeyToAddress
       ) where

import           Universum

import           Pos.Types                        (Address)

import           Pos.Core                         (makePubKeyAddressBoot, unsafeIntegerToCoin)
import           Pos.Crypto                       (SecretKey, toPublic)
import           Pos.Txp                          (TxOut(..))

-- | Factory to create `TxOut`
-- | It is mostly taken from `makeTxOutAux` in `Test.Pos.Client.Txp.UtilSpec`
mkTxOut :: Integer -> Address -> TxOut
mkTxOut amount addr =
    let coin = unsafeIntegerToCoin amount in
    TxOut addr coin

-- | Factory to create an `Address`
-- | Friendly borrowed from `Test.Pos.Client.Txp.UtilSpec`
secretKeyToAddress :: SecretKey -> Address
secretKeyToAddress = makePubKeyAddressBoot . toPublic
