module Test.Spec.BlockMetaScenarios (
    actualBlockMeta
  , blockMetaScenarioA
  , blockMetaScenarioB
  , blockMetaScenarioC
  , blockMetaScenarioD
  , blockMetaScenarioE
  , blockMetaScenarioF
  , blockMetaScenarioG
  , blockMetaScenarioH
  ) where

import           Universum

import qualified Data.Map.Strict as Map

import qualified Data.Set as Set

import           Cardano.Wallet.Kernel.DB.AcidState (DB, dbHdWallets)
import           Cardano.Wallet.Kernel.DB.BlockMeta (AddressMeta (..),
                     BlockMeta (..))
import           Cardano.Wallet.Kernel.DB.HdWallet (HdAccountState (..),
                     hdAccountState, hdUpToDateCheckpoints, hdWalletsAccounts)
import           Cardano.Wallet.Kernel.DB.Spec (checkpointBlockMeta,
                     currentCheckpoint)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet

import           Pos.Core (EpochIndex (..), LocalSlotIndex (..), SlotId (..))
import           Pos.Core.Chrono

import           Test.Infrastructure.Genesis
import           UTxO.Context
import           UTxO.DSL
import           UTxO.Interpreter (BlockMeta' (..))
import           Wallet.Inductive

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

{-------------------------------------------------------------------------------
  Manually written inductives to exercise block metadata in the presence
  of NewPending, ApplyBlock and Rollback
-------------------------------------------------------------------------------}

-- | Extract the current checkpoint BlockMeta from the singleton account in the snapshot.
--
--   NOTE: We assume that our test data will only produce one account in the DB.
actualBlockMeta :: DB -> BlockMeta
actualBlockMeta snapshot' =
    case (theAccount ^. hdAccountState) of
        HdAccountStateUpToDate acc ->
            acc ^. hdUpToDateCheckpoints . currentCheckpoint . checkpointBlockMeta
        _ ->
            error "actualBlockMeta: Expected HdAccountStateUpToDate"
    where
        getOne' ix = fromMaybe (error "Expected a singleton") (IxSet.getOne ix)
        theAccount = getOne' (snapshot' ^. dbHdWallets . hdWalletsAccounts)

-- | A Payment from P0 to P1 with change returned to P0
paymentWithChangeFromP0ToP1 :: forall h. Hash h Addr
                            => GenesisValues h Addr -> Transaction h Addr
paymentWithChangeFromP0ToP1 GenesisValues{..} = Transaction {
         trFresh = 0
       , trIns   = Set.fromList [ fst initUtxoP0 ]
       , trOuts  = [ Output p1 1000
                   , Output p0 (initBalP0 - 1 * (1000 + fee)) -- change
                   ]
       , trFee   = fee
       , trHash  = 1
       , trExtra = []
       }
  where
    fee = overestimate txFee 1 2

-- | Two payments from P0 to P1 with change returned to P0.
--   (t0 uses change address p0 and t1 uses p0b)
--   The second payment spends the change of the first payment.
repeatPaymentWithChangeFromP0ToP1 :: forall h. Hash h Addr
                                  => GenesisValues h Addr
                                  -> Addr
                                  -> (Transaction h Addr, Transaction h Addr)
repeatPaymentWithChangeFromP0ToP1 genVals@GenesisValues{..} changeAddr =
    (t0,t1)
  where
    fee = overestimate txFee 1 2

    t0 = paymentWithChangeFromP0ToP1 genVals
    t1 = Transaction {
            trFresh = 0
          , trIns   = Set.fromList [ Input (hash t0) 1 ]
          , trOuts  = [ Output p1 1000
                      , Output changeAddr (initBalP0 - 1 * (1000 + fee)) -- change
                      ]
          , trFee   = fee
          , trHash  = 2
          , trExtra = []
          }

-- | A payment from P1 to P0 with change returned to P1.
paymentWithChangeFromP1ToP0 :: forall h. Hash h Addr
                            => GenesisValues h Addr -> Transaction h Addr
paymentWithChangeFromP1ToP0 GenesisValues{..} = Transaction {
         trFresh = 0
       , trIns   = Set.fromList [ fst initUtxoP1 ]
       , trOuts  = [ Output p0 1000
                   , Output p1 (initBalP1 - 1 * (1000 + fee)) -- change
                   ]
       , trFee   = fee
       , trHash  = 1
       , trExtra = []
       }
  where
    fee = overestimate txFee 1 2

-- | A payment from P0 to multiple other P0 addresses
paymentWithChangeFromP0ToP0 :: forall h. Hash h Addr
                              => GenesisValues h Addr
                              -> Transaction h Addr
paymentWithChangeFromP0ToP0 GenesisValues{..} = Transaction {
        trFresh = 0
      , trIns   = Set.fromList [ fst initUtxoP0 ]
      , trOuts  = [ Output p0b 1000
                  , Output p0c 1000
                  , Output p0 (initBalP1 - (2*1000 + fee))
                  ]
      , trFee   = fee
      , trHash  = 1
      , trExtra = []
      }
 where
   fee = overestimate txFee 1 2

slot1, slot2 :: SlotId
slot1 = SlotId (EpochIndex 0) (UnsafeLocalSlotIndex 1)
slot2 = SlotId (EpochIndex 0) (UnsafeLocalSlotIndex 2)

-- | Scenario A
-- A single pending payment from P0 to P1, with 'change' returned to P0
--
-- This scenario shows that a for an address to be considered 'change', it must be
-- part of a confirmed transaction
blockMetaScenarioA :: forall h. Hash h Addr
                   => GenesisValues h Addr
                   -> (Inductive h Addr, BlockMeta' h)
blockMetaScenarioA genVals@GenesisValues{..} =
    (ind, BlockMeta'{..})
  where
    t0 = paymentWithChangeFromP0ToP1 genVals
    ind = Inductive {
          inductiveBoot   = boot
        , inductiveOurs   = Set.singleton p0 -- define the owner of the wallet: Poor actor 0
        , inductiveEvents = OldestFirst [
              NewPending t0
            ]
        }

    --  EXPECTED BlockMeta:
    --    * since the transaction is not confirmed, we expect no confirmed transactions
    _blockMetaSlotId' = Map.empty
    --    * we expect no address metadata for the pending 'change' address
    _blockMetaAddressMeta' = Map.empty

-- | Scenario B
-- A single pending payment from P0 to P1, with 'change' returned to P0
--
-- This scenario shows that an address is considered change when all the requirements are met:
--   the address must occur in exactly one confirmed transaction, for which all inputs
--   are "ours" but not all outputs are "ours"
blockMetaScenarioB :: forall h. Hash h Addr
                   => GenesisValues h Addr
                   -> (Inductive h Addr, BlockMeta' h)
blockMetaScenarioB genVals@GenesisValues{..} =
    (ind, BlockMeta'{..})
  where
    t0 = paymentWithChangeFromP0ToP1 genVals
    ind = Inductive {
          inductiveBoot   = boot
        , inductiveOurs   = Set.singleton p0 -- define the owner of the wallet: Poor actor 0
        , inductiveEvents = OldestFirst [
              NewPending t0
            , ApplyBlock $ OldestFirst [t0] -- confirms t0 and updates block metadata
            ]
        }

    --  EXPECTED BlockMeta:
    --    * since the transaction is now confirmed, we expect to see the single transaction
    _blockMetaSlotId' = Map.singleton (hash t0) slot1
    --    * we expect the address to be recognised as a 'change' address in the metadata
    _blockMetaAddressMeta'
        = Map.singleton p0 (AddressMeta {_addressMetaIsUsed = True, _addressMetaIsChange = True})

-- | Scenario C
-- Two confirmed payments from P0 to P1, using `change` addresses P0 and P0b respectively
--
-- This scenario shows multiple change addresses co-existing
blockMetaScenarioC :: forall h. Hash h Addr
                   => GenesisValues h Addr
                   -> (Inductive h Addr, BlockMeta' h)
blockMetaScenarioC genVals@GenesisValues{..} =
    (ind, BlockMeta'{..})
  where
    (t0,t1) = repeatPaymentWithChangeFromP0ToP1 genVals p0b
    ind = Inductive {
          inductiveBoot   = boot
        , inductiveOurs   = Set.fromList [p0,p0b] -- define the owner of the wallet: Poor actor 0
        , inductiveEvents = OldestFirst [
              NewPending t0
            , ApplyBlock $ OldestFirst [t0] -- confirms t0 and updates block metadata
            , ApplyBlock $ OldestFirst [t1] -- confirms t1 and updates block metadata
            ]
        }

    --  EXPECTED BlockMeta:
    --    * we expect to see the 2 confirmed transactions
    _blockMetaSlotId' = Map.fromList [(hash t0, slot1),(hash t1, slot2)]
    --    * we expect both addresses to be recognised as 'change' addresses in the metadata
    --      (since each address occur in exactly one confirmed transaction, and all the other requirements are met)
    _blockMetaAddressMeta'
        = Map.fromList [  (p0,  (AddressMeta {_addressMetaIsUsed = True, _addressMetaIsChange = True}))
                        , (p0b, (AddressMeta {_addressMetaIsUsed = True, _addressMetaIsChange = True}))]

-- | Scenario D
-- ScenarioC + Rollback
--
-- This scenario exercises Rollback behaviour for block metadata
blockMetaScenarioD :: forall h. Hash h Addr
                   => GenesisValues h Addr
                   -> (Inductive h Addr, BlockMeta' h)
blockMetaScenarioD genVals@GenesisValues{..} =
    (ind, BlockMeta'{..})
  where
    (t0,t1) = repeatPaymentWithChangeFromP0ToP1 genVals p0b
    ind = Inductive {
          inductiveBoot   = boot
        , inductiveOurs   = Set.fromList [p0,p0b] -- define the owner of the wallet: Poor actor 0
        , inductiveEvents = OldestFirst [
              NewPending t0
            , ApplyBlock $ OldestFirst [t0] -- confirms t0 and updates block metadata
            , ApplyBlock $ OldestFirst [t1] -- confirms t1 and updates block metadata
            , Rollback                      -- rolls back t1 and updates block metadata
            ]
        }

    --  EXPECTED BlockMeta:
    --    * we expect to see only 1 confirmed transaction after the rollback
    _blockMetaSlotId' = Map.singleton (hash t0) slot1
    --    * we expect the first address to still be recognised as a 'change' address in the metadata
    _blockMetaAddressMeta'
        = Map.singleton p0 (AddressMeta {_addressMetaIsUsed = True, _addressMetaIsChange = True})

-- | Scenario E
-- Two confirmed payments from P0 to P1, both using the same `change` address for P0
--
-- This scenario shows that for an address to be considered 'change',
-- the address must occur in exactly one confirmed transaction
blockMetaScenarioE :: forall h. Hash h Addr
                   => GenesisValues h Addr
                   -> (Inductive h Addr, BlockMeta' h)
blockMetaScenarioE genVals@GenesisValues{..} =
    (ind, BlockMeta'{..})
  where
    (t0,t1) = repeatPaymentWithChangeFromP0ToP1 genVals p0
    ind = Inductive {
          inductiveBoot   = boot
        , inductiveOurs   = Set.fromList [p0,p0b] -- define the owner of the wallet: Poor actor 0
        , inductiveEvents = OldestFirst [
              NewPending t0
            , ApplyBlock $ OldestFirst [t0] -- confirms t0 and updates block metadata
            , ApplyBlock $ OldestFirst [t1] -- confirms t1 and updates block metadata
            ]
        }

    --  EXPECTED BlockMeta:
    --    * we expect to see the 2 confirmed transactions
    _blockMetaSlotId' = Map.fromList [(hash t0, slot1),(hash t1, slot2)]
    --    * we expect the address to no longer be recognised as a 'change' address in the metadata
    --      (because a `change` address must occur in exactly one confirmed transaction)
    _blockMetaAddressMeta'
        = Map.singleton p0 (AddressMeta {_addressMetaIsUsed = True, _addressMetaIsChange = False})

-- | Scenario F
-- ScenarioE + Rollback
--
-- This scenario exercises Rollback behaviour for block metadata
blockMetaScenarioF :: forall h. Hash h Addr
                   => GenesisValues h Addr
                   -> (Inductive h Addr, BlockMeta' h)
blockMetaScenarioF genVals@GenesisValues{..} =
    (ind, BlockMeta'{..})
  where
    (t0,t1) = repeatPaymentWithChangeFromP0ToP1 genVals p0
    ind = Inductive {
          inductiveBoot   = boot
        , inductiveOurs   = Set.fromList [p0,p0b] -- define the owner of the wallet: Poor actor 0
        , inductiveEvents = OldestFirst [
              NewPending t0
            , ApplyBlock $ OldestFirst [t0] -- confirms t0 and updates block metadata
            , ApplyBlock $ OldestFirst [t1] -- confirms t1 and updates block metadata
            , Rollback                      -- rolls back t1 and updates block metadata
            ]
        }

    --  EXPECTED BlockMeta:
    --    * we expect to see only 1 confirmed transaction after the rollback
    _blockMetaSlotId' = Map.singleton (hash t0) slot1
    --    * we expect the address to again be recognised as a 'change' address in the metadata
    --      (the rollback leads to the `change` address occuring in exactly one confirmed transaction again, as in ScenarioE)
    _blockMetaAddressMeta'
        = Map.singleton p0 (AddressMeta {_addressMetaIsUsed = True, _addressMetaIsChange = True})

-- | Scenario G
-- A payment from P1 to P0's single address.

-- This scenario asserts the requirement for an address to be considered 'change',
-- the address must occur in a single confirmed transaction for which all inputs are "ours"
blockMetaScenarioG :: forall h. Hash h Addr
                   => GenesisValues h Addr
                   -> (Inductive h Addr, BlockMeta' h)
blockMetaScenarioG genVals@GenesisValues{..} =
    (ind, BlockMeta'{..})
  where
    t0 = paymentWithChangeFromP1ToP0 genVals
    ind = Inductive {
          inductiveBoot   = boot
        , inductiveOurs   = Set.singleton p0 -- define the owner of the wallet: Poor actor 0
        , inductiveEvents = OldestFirst [
            ApplyBlock $ OldestFirst [t0] -- confirms t0 and updates block metadata
            ]
        }

    --  EXPECTED BlockMeta:
    --    * we expect to see the single confirmed transaction
    _blockMetaSlotId' = Map.singleton (hash t0) slot1
    -- For `t0` the inputs are not all "ours" and hence `isChange` is False
    _blockMetaAddressMeta'
        = Map.singleton p0 (AddressMeta {_addressMetaIsUsed = True, _addressMetaIsChange = False})

-- | Scenario G
-- A confirmed payment from P0 to a single other P0 address
--
-- This scenario asserts the requirement for an address to be considered 'change',
-- the address must occur in a single confirmed transaction for which _not_ all outputs are "ours"
blockMetaScenarioH :: forall h. Hash h Addr
                   => GenesisValues h Addr
                   -> (Inductive h Addr, BlockMeta' h)
blockMetaScenarioH genVals@GenesisValues{..} =
    (ind, BlockMeta'{..})
  where
    t0 = paymentWithChangeFromP0ToP0 genVals
    ind = Inductive {
          inductiveBoot   = boot
        , inductiveOurs   = Set.fromList [p0,p0b,p0c] -- define the owner of the wallet: Poor actor 0
        , inductiveEvents = OldestFirst [
              NewPending t0
            , ApplyBlock $ OldestFirst [t0] -- confirms t0 and updates block metadata
            ]
        }

    --  EXPECTED BlockMeta:
    --    * we expect to see the confirmed transaction
    _blockMetaSlotId' = Map.singleton (hash t0) slot1
    --    * we expect none of the addresses to be recognised as 'change'
    --      (since all the transaction outputs are to addresses that are _all_ "ours")
    _blockMetaAddressMeta'
        = Map.fromList [  (p0,  (AddressMeta {_addressMetaIsUsed = True, _addressMetaIsChange = False}))
                        , (p0b, (AddressMeta {_addressMetaIsUsed = True, _addressMetaIsChange = False}))
                        , (p0c, (AddressMeta {_addressMetaIsUsed = True, _addressMetaIsChange = False}))]
