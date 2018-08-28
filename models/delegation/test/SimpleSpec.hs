module SimpleSpec where

import           Crypto.Hash (hash)
import qualified Data.Map   as Map
import qualified Data.Set   as Set
import           Test.Hspec

import           Data.List  (find)
import           Delegation


alice = Owner 1
bob = Owner 2
carol = Owner 3

aliceAddr = AddrTxin (hash (VKey alice)) (hash (SKey alice))
bobAddr = AddrTxin (hash (VKey bob)) (hash (SKey bob))
carolAddr = AddrTxin (hash (VKey carol)) (hash (SKey carol))

noWitnesses = Wits Set.empty Set.empty

tx0 = Tx
        (TxBody (Set.empty)
          [ TxOut aliceAddr (Coin 10)
          , TxOut bobAddr (Coin 20) ]
          Set.empty)
        noWitnesses
tx0Id = txid tx0

genesisUTxO = txouts tx0

tx1Body = TxBody
            (Set.fromList [TxIn tx0Id 0])
            [ TxOut aliceAddr (Coin 7)
            , TxOut bobAddr (Coin 1)
            , TxOut carolAddr (Coin 2) ]
            (Set.fromList [Delegate])
tx1Wits = Wits
            (Set.fromList
              [ WitTxin (VKey alice) (Sig (TxIn tx0Id 0) alice)
              ])
            (Set.fromList
              [ WitCert (VKey alice) (Sig Delegate alice)
              ])
tx1 = Tx tx1Body tx1Wits

spec :: Spec
spec = do
  it "transaction transition" $
    transactionTransition tx1 genesisUTxO `shouldBe` Right ( UTxO ( Map.fromList
      [ (TxIn (txid tx0) 1, TxOut bobAddr (Coin 20))
      , (TxIn (txid tx1) 0, TxOut aliceAddr (Coin 7))
      , (TxIn (txid tx1) 1, TxOut bobAddr (Coin 1))
      , (TxIn (txid tx1) 2, TxOut carolAddr (Coin 2))
      ]))
  it "genesis balance" $ balance genesisUTxO `shouldBe` Coin 30
  it "correct witnesses" $ witness tx1 genesisUTxO `shouldBe` True
