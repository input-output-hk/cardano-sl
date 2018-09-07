{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module SimpleSpec where

import           Crypto.Hash (hash)
import qualified Data.Map    as Map
import qualified Data.Set    as Set
import           Test.Hspec

import           Delegation

alicePay = keyPair (Owner 1)
aliceStake = keyPair (Owner 2)
bobPay = keyPair (Owner 3)
bobStake = keyPair (Owner 4)
poolPay = keyPair (Owner 5)
poolStake = keyPair (Owner 6)

aliceAddr = AddrTxin (hash (vKey alicePay)) (hash (vKey aliceStake))
bobAddr = AddrTxin (hash (vKey bobPay)) (hash (vKey bobStake))
poolAddr = AddrTxin (hash (vKey poolPay)) (hash (vKey poolStake))

aliceStKeyHash = hashKey (vKey aliceStake)
bobStKeyHash = hashKey (vKey bobStake)
poolStKeyHash = hashKey (vKey poolStake)

genesis = genesisState
            [ TxOut aliceAddr (Coin 10)
            , TxOut bobAddr (Coin 20) ]

tx1Body = TxBody
            (Set.fromList [TxIn genesisId 0])
            [ TxOut aliceAddr (Coin 7)
            , TxOut bobAddr (Coin 1)
            , TxOut poolAddr (Coin 2) ]
            (Set.fromList
              [ RegKey (vKey aliceStake)
              , RegKey (vKey bobStake)
              , RegKey (vKey poolStake)])
tx1Wits = Wits
            (Set.fromList
              [ WitTxin (vKey alicePay) (sign (sKey alicePay) tx1Body)
              ])
            (Set.fromList
              [ WitCert (vKey aliceStake) (sign (sKey  aliceStake) tx1Body)
              , WitCert (vKey bobStake) (sign (sKey  bobStake) tx1Body)
              , WitCert (vKey poolStake) (sign (sKey  poolStake) tx1Body)
              ])
tx1 = Tx tx1Body tx1Wits

tx2Body = TxBody Set.empty []
            (Set.fromList
               [ RegPool $ StakePool (vKey poolStake) Map.empty (Coin 1) 0.01 Nothing
               , RegPool $ StakePool (vKey bobStake) Map.empty (Coin 0) 1 Nothing])
tx2Wits = Wits
            Set.empty
            (Set.fromList
              [ WitCert (vKey poolStake) (sign (sKey  poolStake) tx2Body)
              , WitCert (vKey bobStake) (sign (sKey  bobStake) tx2Body)
              ])
tx2 = Tx tx2Body tx2Wits

tx3Body = TxBody Set.empty []
            (Set.fromList
               [ Delegate (Delegation (vKey aliceStake) (vKey poolStake))
               , Delegate (Delegation (vKey bobStake) (vKey bobStake))
               , Delegate (Delegation (vKey poolStake) (vKey poolStake))])
tx3Wits = Wits
            Set.empty
            (Set.fromList
              [ WitCert (vKey aliceStake) (sign (sKey  aliceStake) tx3Body)
              , WitCert (vKey bobStake) (sign (sKey  bobStake) tx3Body)
              , WitCert (vKey poolStake) (sign (sKey  poolStake) tx3Body)
              ])
tx3 = Tx tx3Body tx3Wits

tx4Body = TxBody Set.empty [] (Set.fromList [DeRegKey (vKey aliceStake)])
tx4Wits = Wits Set.empty
            (Set.fromList [WitCert (vKey aliceStake) (sign (sKey  aliceStake) tx4Body)])
tx4 = Tx tx4Body tx4Wits

tx5Body = TxBody Set.empty []
            (Set.fromList [Delegate (Delegation (vKey bobStake) (vKey poolStake))])
tx5Wits = Wits Set.empty
            (Set.fromList [WitCert (vKey bobStake) (sign (sKey  bobStake) tx5Body)])
tx5 = Tx tx5Body tx5Wits

tx6Body = TxBody Set.empty []
            (Set.fromList [RetirePool (vKey poolStake) 5])
tx6Wits = Wits Set.empty
            (Set.fromList [WitCert (vKey poolStake) (sign (sKey  poolStake) tx6Body)])
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
