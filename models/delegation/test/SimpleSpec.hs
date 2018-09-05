{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module SimpleSpec where

import           Crypto.Hash (hash)
import qualified Data.Map    as Map
import qualified Data.Set    as Set
import           Test.Hspec

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

aliceStKeyHash = hashKey (VKey aliceStake)
bobStKeyHash = hashKey (VKey bobStake)
poolStKeyHash = hashKey (VKey poolStake)

genesis = genesisState
            [ TxOut aliceAddr (Coin 10)
            , TxOut bobAddr (Coin 20) ]

tx1Body = TxBody
            (Set.fromList [TxIn genesisId 0])
            [ TxOut aliceAddr (Coin 7)
            , TxOut bobAddr (Coin 1)
            , TxOut poolAddr (Coin 2) ]
            (Set.fromList
              [ RegKey (VKey aliceStake)
              , RegKey (VKey bobStake)
              , RegKey (VKey poolStake)])
tx1Wits = Wits
            (Set.fromList
              [ WitTxin (VKey alicePay) (Sig tx1Body alicePay)
              ])
            (Set.fromList
              [ WitCert (VKey aliceStake) (Sig tx1Body aliceStake)
              , WitCert (VKey bobStake) (Sig tx1Body bobStake)
              , WitCert (VKey poolStake) (Sig tx1Body poolStake)
              ])
tx1 = Tx tx1Body tx1Wits

tx2Body = TxBody Set.empty []
            (Set.fromList
               [ RegPool $ StakePool (VKey poolStake) Map.empty (Coin 1) 0.01 Nothing
               , RegPool $ StakePool (VKey bobStake) Map.empty (Coin 0) 1 Nothing])
tx2Wits = Wits
            Set.empty
            (Set.fromList
              [ WitCert (VKey poolStake) (Sig tx2Body poolStake)
              , WitCert (VKey bobStake) (Sig tx2Body bobStake)
              ])
tx2 = Tx tx2Body tx2Wits

tx3Body = TxBody Set.empty []
            (Set.fromList
               [ Delegate (Delegation (VKey aliceStake) (VKey poolStake))
               , Delegate (Delegation (VKey bobStake) (VKey bobStake))
               , Delegate (Delegation (VKey poolStake) (VKey poolStake))])
tx3Wits = Wits
            Set.empty
            (Set.fromList
              [ WitCert (VKey aliceStake) (Sig tx3Body aliceStake)
              , WitCert (VKey bobStake) (Sig tx3Body bobStake)
              , WitCert (VKey poolStake) (Sig tx3Body poolStake)
              ])
tx3 = Tx tx3Body tx3Wits

tx4Body = TxBody Set.empty [] (Set.fromList [DeRegKey (VKey aliceStake)])
tx4Wits = Wits Set.empty
            (Set.fromList [WitCert (VKey aliceStake) (Sig tx4Body aliceStake)])
tx4 = Tx tx4Body tx4Wits

tx5Body = TxBody Set.empty []
            (Set.fromList [Delegate (Delegation (VKey bobStake) (VKey poolStake))])
tx5Wits = Wits Set.empty
            (Set.fromList [WitCert (VKey bobStake) (Sig tx5Body bobStake)])
tx5 = Tx tx5Body tx5Wits

tx6Body = TxBody Set.empty []
            (Set.fromList [RetirePool (VKey poolStake) 5])
tx6Wits = Wits Set.empty
            (Set.fromList [WitCert (VKey poolStake) (Sig tx6Body poolStake)])
tx6 = Tx tx6Body tx6Wits

ledgerState :: Ledger -> Either [ValidationError] LedgerState
ledgerState []     = Right $ genesis
ledgerState (t:ts) = ledgerState ts >>= asStateTransition t

expectedUtxo = UTxO $ Map.fromList
                 [ (TxIn genesisId 1, TxOut bobAddr (Coin 20))
                 , (TxIn (txid tx1) 0, TxOut aliceAddr (Coin 7))
                 , (TxIn (txid tx1) 1, TxOut bobAddr (Coin 1))
                 , (TxIn (txid tx1) 2, TxOut poolAddr (Coin 2))
                 ]

spec :: Spec
spec = do
  it "genesis balance" $ balance (getUtxo genesis) `shouldBe` Coin 30
  it "transaction as state transition" $
    ledgerState [tx1] `shouldBe` Right ( LedgerState
      { getUtxo = expectedUtxo
      , getAccounts = Map.fromList
                        [ (aliceStKeyHash, Coin 0)
                        , (bobStKeyHash, Coin 0)
                        , (poolStKeyHash, Coin 0)
                        ]
      , getStKeys = Set.fromList [aliceStKeyHash, bobStKeyHash, poolStKeyHash]
      , getDelegations = Map.empty
      , getStPools = Set.empty
      , getRetiring = Map.empty
      , getEpoch = 0
      })
  it "delegation" $
    ledgerState [tx3, tx2, tx1] `shouldBe` Right ( LedgerState
      { getUtxo = expectedUtxo
      , getAccounts = Map.fromList
                        [ (aliceStKeyHash, Coin 0)
                        , (bobStKeyHash, Coin 0)
                        , (poolStKeyHash, Coin 0)
                        ]
      , getStKeys = Set.fromList [aliceStKeyHash, bobStKeyHash, poolStKeyHash]
      , getDelegations = Map.fromList
                           [ (aliceStKeyHash, poolStKeyHash)
                           , (bobStKeyHash, bobStKeyHash)
                           , (poolStKeyHash, poolStKeyHash)]
      , getStPools = Set.fromList [poolStKeyHash, bobStKeyHash]
      , getRetiring = Map.empty
      , getEpoch = 0
      })
  it "delegated stake" $
    let Right ls = ledgerState [tx3, tx2, tx1] in
      delegatedStake ls `shouldBe` Map.fromList
                                   [ (poolStKeyHash, Coin 9)
                                   , (bobStKeyHash, Coin 21)
                                   ]
  it "deregister stake key" $
    ledgerState [tx4, tx3, tx2, tx1] `shouldBe` Right ( LedgerState
      { getUtxo = expectedUtxo
      , getAccounts = Map.fromList
                        [ (bobStKeyHash, Coin 0)
                        , (poolStKeyHash, Coin 0)
                        ]
      , getStKeys = Set.fromList [bobStKeyHash, poolStKeyHash]
      , getDelegations = Map.fromList
                           [ (bobStKeyHash, bobStKeyHash)
                           , (poolStKeyHash, poolStKeyHash)]
      , getStPools = Set.fromList [poolStKeyHash, bobStKeyHash]
      , getRetiring = Map.empty
      , getEpoch = 0
      })

  it "re-delegate" $
    ledgerState [tx5, tx4, tx3, tx2, tx1] `shouldBe` Right ( LedgerState
      { getUtxo = expectedUtxo
      , getAccounts = Map.fromList
                        [ (bobStKeyHash, Coin 0)
                        , (poolStKeyHash, Coin 0)
                        ]
      , getStKeys = Set.fromList [bobStKeyHash, poolStKeyHash]
      , getDelegations = Map.fromList
                           [ (bobStKeyHash, poolStKeyHash)
                           , (poolStKeyHash, poolStKeyHash)]
      , getStPools = Set.fromList [poolStKeyHash, bobStKeyHash]
      , getRetiring = Map.empty
      , getEpoch = 0
      })

  it "retiring a pool" $
    ledgerState [tx6, tx5, tx4, tx3, tx2, tx1] `shouldBe` Right ( LedgerState
      { getUtxo = expectedUtxo
      , getAccounts = Map.fromList
                        [ (bobStKeyHash, Coin 0)
                        , (poolStKeyHash, Coin 0)
                        ]
      , getStKeys = Set.fromList [bobStKeyHash, poolStKeyHash]
      , getDelegations = Map.fromList
                           [ (bobStKeyHash, poolStKeyHash)
                           , (poolStKeyHash, poolStKeyHash)]
      , getStPools = Set.fromList [poolStKeyHash, bobStKeyHash]
      , getRetiring = Map.fromList [(poolStKeyHash, 5)]
      , getEpoch = 0
      })
  it "retire pool" $
    let (Right ls) = ledgerState [tx6, tx5, tx4, tx3, tx2, tx1] in
      (retirePools ls 5) `shouldBe` LedgerState {
        getUtxo = expectedUtxo
        , getAccounts = Map.fromList
                          [ (bobStKeyHash, Coin 0)
                          , (poolStKeyHash, Coin 0)
                          ]
        , getStKeys = Set.fromList [bobStKeyHash, poolStKeyHash]
        , getDelegations = Map.fromList
                             [ (bobStKeyHash, poolStKeyHash)
                             , (poolStKeyHash, poolStKeyHash)]
        , getStPools = Set.fromList [bobStKeyHash]
        , getRetiring = Map.empty
        , getEpoch = 0
        }
