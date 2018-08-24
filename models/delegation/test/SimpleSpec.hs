module SimpleSpec where

import qualified Data.Map   as Map
import qualified Data.Set   as Set
import           Test.Hspec

import           Delegation


tx0 = Tx (Set.fromList [])
        [ TxOut (Addr 1) (Coin 10)
        , TxOut (Addr 2) (Coin 20) ]

genesisUTxO = txouts tx0

tx1 = Tx (Set.fromList [TxIn (txid tx0) 0])
        [ TxOut (Addr 1) (Coin 7)
        , TxOut (Addr 2) (Coin 1)
        , TxOut (Addr 3) (Coin 2) ]

spec :: Spec
spec = do
  it "transaction transition" $
    transactionTransition tx1 genesisUTxO `shouldBe` Right ( UTxO ( Map.fromList
      [ (TxIn (txid tx0) 1, TxOut (Addr 2) (Coin 20))
      , (TxIn (txid tx1) 0, TxOut (Addr 1) (Coin 7))
      , (TxIn (txid tx1) 1, TxOut (Addr 2) (Coin 1))
      , (TxIn (txid tx1) 2, TxOut (Addr 3) (Coin 2))
      ]))
  it "genesis balance" $ balance genesisUTxO `shouldBe` Coin 30
