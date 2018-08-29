module SimpleSpec where

import           Crypto.Hash (hash)
import qualified Data.Map   as Map
import qualified Data.Set   as Set
import           Test.Hspec

import           Data.List  (find)
import           Delegation


alice_pay = Owner 1
alice_stake = Owner 2
bob_pay = Owner 3
bob_stake = Owner 4
carol_pay = Owner 5
carol_stake = Owner 6

aliceAddr = AddrTxin (hash (VKey alice_pay)) (hash (VKey alice_stake))
bobAddr = AddrTxin (hash (VKey bob_pay)) (hash (VKey bob_stake))
carolAddr = AddrTxin (hash (VKey carol_pay)) (hash (VKey carol_stake))

noWitnesses = Wits Set.empty Set.empty

tx0 = Tx
        (TxBody (Set.empty)
          [ TxOut aliceAddr (Coin 10)
          , TxOut bobAddr (Coin 20) ]
          Set.empty)
        noWitnesses
tx0Id = txid tx0

genesisState = applyTransaction tx0 emptyLedgerState

tx1Body = TxBody
            (Set.fromList [TxIn tx0Id 0])
            [ TxOut aliceAddr (Coin 7)
            , TxOut bobAddr (Coin 1)
            , TxOut carolAddr (Coin 2) ]
            (Set.fromList [Delegate])
tx1Wits = Wits
            (Set.fromList
              [ WitTxin (VKey alice_pay) (Sig tx1Body alice_pay)
              ])
            (Set.fromList
              [ WitCert (VKey alice_stake) (Sig tx1Body alice_stake)
              ])
tx1 = Tx tx1Body tx1Wits

spec :: Spec
spec = do
  it "genesis balance" $ balance (getUtxo genesisState) `shouldBe` Coin 30
  it "transaction transition" $
    asStateTransition tx1 genesisState `shouldBe` Right ( LedgerState {
      getUtxo = UTxO $ Map.fromList
                  [ (TxIn (txid tx0) 1, TxOut bobAddr (Coin 20))
                  , (TxIn (txid tx1) 0, TxOut aliceAddr (Coin 7))
                  , (TxIn (txid tx1) 1, TxOut bobAddr (Coin 1))
                  , (TxIn (txid tx1) 2, TxOut carolAddr (Coin 2))
                  ]
      , getAccounts = Map.empty
      , getStKeys = Set.empty
      , getDelegations = Map.empty
      , getStPools = Set.empty
      , getRetiring = Map.empty
      , getEpoch = 0
      })
