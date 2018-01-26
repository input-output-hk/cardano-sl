
-- | Factory to create mock objects
-- | It might be merged with other factories or test utilities of others modules
-- | such as `Test.Pos.Client.Txp.UtilSpec`.

module Test.Pos.Explorer.MockFactory
       ( mkTxOut
       , testLoggerName
       ) where

import           Universum

import           System.Wlog (LoggerName)

import           Pos.Core (Address, unsafeIntegerToCoin)
import           Pos.Core.Txp (TxOut (..))

-- | Factory to create `TxOut`
-- | It is mostly taken from `makeTxOutAux` in `Test.Pos.Client.Txp.UtilSpec`
mkTxOut :: Integer -> Address -> TxOut
mkTxOut amount addr =
    let coin = unsafeIntegerToCoin amount in
    TxOut addr coin

-- | Logger name for testing
testLoggerName :: LoggerName
testLoggerName = "test"
