module SimpleSpec where

import           Crypto.Hash (hash)
import qualified Data.Map   as Map
import qualified Data.Set   as Set
import           Test.Hspec

import           Data.List  (find)
import           Delegation


alicePay = Owner 1
aliceStake = Owner 2
bobPay = Owner 3
bobStake = Owner 4
poolPay = Owner 5
poolStake = Owner 6

aliceAddr = AddrTxin (hash (VKey alicePay)) (hash (VKey aliceStake))
bobAddr = AddrTxin (hash (VKey bobPay)) (hash (VKey bobStake))
poolAddr = AddrTxin (hash (VKey poolPay)) (hash (VKey poolStake))

genesis = genesisState
            [ TxOut aliceAddr (Coin 10)
            , TxOut bobAddr (Coin 20) ]

tx1Body = TxBody
            (Set.fromList [TxIn genesisId 0])
            [ TxOut aliceAddr (Coin 7)
            , TxOut bobAddr (Coin 1)
            , TxOut poolAddr (Coin 2) ]
            (Set.fromList [
              Delegate (Delegation (VKey aliceStake) (VKey poolStake))])
tx1Wits = Wits
            (Set.fromList
              [ WitTxin (VKey alicePay) (Sig tx1Body alicePay)
              ])
            (Set.fromList
              [ WitCert (VKey aliceStake) (Sig tx1Body aliceStake)
              ])
tx1 = Tx tx1Body tx1Wits

spec :: Spec
spec = do
  it "genesis balance" $ balance (getUtxo genesis) `shouldBe` Coin 30
  it "transaction transition" $
    asStateTransition genesis tx1 `shouldBe` Right ( LedgerState {
      getUtxo = UTxO $ Map.fromList
                  [ (TxIn genesisId 1, TxOut bobAddr (Coin 20))
                  , (TxIn (txid tx1) 0, TxOut aliceAddr (Coin 7))
                  , (TxIn (txid tx1) 1, TxOut bobAddr (Coin 1))
                  , (TxIn (txid tx1) 2, TxOut poolAddr (Coin 2))
                  ]
      , getAccounts = Map.empty
      , getStKeys = Set.empty
      , getDelegations = Map.fromList [
          ((hashKey (VKey aliceStake)), (hashKey (VKey poolStake)))]
      , getStPools = Set.empty
      , getRetiring = Map.empty
      , getEpoch = 0
      })
