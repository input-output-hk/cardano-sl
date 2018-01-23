-- | Generate bootstrap transactions for the DSL
module UTxO.Bootstrap (
    bootstrapTransactions
  , isBootstrapTransaction
  ) where

import Universum
import Data.Bifunctor (bimap)

import Pos.Core

import UTxO.Context
import qualified UTxO.DSL as DSL

-- | Construct the bootstrap transactions
--
-- In UTxO-style accounting transactions list as input other transactions. This
-- begs the question what happens at the start of the blockchain where there are
-- no transactions yet -- which transactions do the first transactions refer to?
-- The DSL and Cardano core have two different solutions to this:
--
-- * In the DSL a transaction can have an empty list of inputs
-- * In Cardano transaction lists transaction _hashes_ as their inputs, rather
--   than transactions proper. For the initial transaction this instead lists
--   the hash of the address that has the initial balance.
--
-- So what does a Cardano transaction "move X from A's initial balance to B"
-- look like in the DSL? In the DSL empty transactions create new coins and
-- can only output to the treasury; so the equivalent is something like this:
--
-- > let t0 = Transaction {
-- >              trIns  = []
-- >            , trOuts = [Output AddrTreasury TotalAmountOfAda]
-- >            }
-- >     t1 = Transaction {
-- >              trIns  = [ (t0, 0) ]
-- >            , trOuts = [ Output A InitialBalanceA
-- >                       , Output B InitialBalanceB
-- >                       , ...
-- >                       ]
-- >            }
-- >     t2 = Transaction {
-- >              trIns  = [ (t1, 1)] ]
-- >            , trOuts = [ Output B X ]
-- >            }
--
-- where neither @t0@ nor @t1@ correspond to an actual transaction in Cardano.
-- We need to set things up like this because functions that operate purely
-- on the DSL will assume things to be set up like they are expected in the
-- paper. This means that we need to infer t0 and t1 from the generated
-- genesis block and make it available as an input for transactions in the
-- tests. In the translation we then need to recognize this special transaction
-- and deal with appropriately.
--
-- (Alternatively it would be equivalent to generate a bunch of transactions to
-- set up the initial balances for all actors, but if we did that we'd need a
-- whole chain of transactions to distribute the balances.)
--
-- For the specific context generated in the test genesis block, we generate
--
-- > let t0 = Transaction
-- >           { trIns = []
-- >           , trOuts = [ Output { outAddr = AddrTreasury , outVal = 44999999999999992 } ]
-- >           }
-- > in [t0, Transaction
-- >     { trIns = [ Input { inpTrans = t0, inpIndex = 0 } ]
-- >     , trOuts =
-- >         [ Output { outAddr = AddrOrdinary Addr { addrActorIx = IxRich 0 , addrIx = 0 }, outVal = 11137499999752500 }
-- >         , Output { outAddr = AddrOrdinary Addr { addrActorIx = IxRich 1 , addrIx = 0 }, outVal = 11137499999752500 }
-- >         , Output { outAddr = AddrOrdinary Addr { addrActorIx = IxRich 2 , addrIx = 0 }, outVal = 11137499999752500 }
-- >         , Output { outAddr = AddrOrdinary Addr { addrActorIx = IxRich 3 , addrIx = 0 }, outVal = 11137499999752500 }
-- >         , Output { outAddr = AddrOrdinary Addr { addrActorIx = IxPoor 0 , addrIx = 0 }, outVal = 37499999999166 }
-- >         , Output { outAddr = AddrOrdinary Addr { addrActorIx = IxPoor 1 , addrIx = 0 }, outVal = 37499999999166 }
-- >         , Output { outAddr = AddrOrdinary Addr { addrActorIx = IxPoor 2 , addrIx = 0 }, outVal = 37499999999166 }
-- >         , Output { outAddr = AddrOrdinary Addr { addrActorIx = IxPoor 3 , addrIx = 0 }, outVal = 37499999999166 }
-- >         , Output { outAddr = AddrOrdinary Addr { addrActorIx = IxPoor 4 , addrIx = 0 }, outVal = 37499999999166 }
-- >         , Output { outAddr = AddrOrdinary Addr { addrActorIx = IxPoor 5 , addrIx = 0 }, outVal = 37499999999166 }
-- >         , Output { outAddr = AddrOrdinary Addr { addrActorIx = IxPoor 6 , addrIx = 0 }, outVal = 37499999999166 }
-- >         , Output { outAddr = AddrOrdinary Addr { addrActorIx = IxPoor 7 , addrIx = 0 }, outVal = 37499999999166 }
-- >         , Output { outAddr = AddrOrdinary Addr { addrActorIx = IxPoor 8 , addrIx = 0 }, outVal = 37499999999166 }
-- >         , Output { outAddr = AddrOrdinary Addr { addrActorIx = IxPoor 9 , addrIx = 0 }, outVal = 37499999999166 }
-- >         , Output { outAddr = AddrOrdinary Addr { addrActorIx = IxPoor 10 , addrIx = 0 }, outVal = 37499999999166 }
-- >         , Output { outAddr = AddrOrdinary Addr { addrActorIx = IxPoor 11 , addrIx = 0 }, outVal = 37499999999166 }
-- >         , Output { outAddr = AddrOrdinary Addr { addrActorIx = IxAvvm 0 , addrIx = 0 }, outVal = 100000 }
-- >         , Output { outAddr = AddrOrdinary Addr { addrActorIx = IxAvvm 1 , addrIx = 0 }, outVal = 100000 }
-- >         , Output { outAddr = AddrOrdinary Addr { addrActorIx = IxAvvm 2 , addrIx = 0 }, outVal = 100000 }
-- >         , Output { outAddr = AddrOrdinary Addr { addrActorIx = IxAvvm 3 , addrIx = 0 }, outVal = 100000 }
-- >         , Output { outAddr = AddrOrdinary Addr { addrActorIx = IxAvvm 4 , addrIx = 0 }, outVal = 100000 }
-- >         , Output { outAddr = AddrOrdinary Addr { addrActorIx = IxAvvm 5 , addrIx = 0 }, outVal = 100000 }
-- >         , Output { outAddr = AddrOrdinary Addr { addrActorIx = IxAvvm 6 , addrIx = 0 }, outVal = 100000 }
-- >         , Output { outAddr = AddrOrdinary Addr { addrActorIx = IxAvvm 7 , addrIx = 0 }, outVal = 100000 }
-- >         , Output { outAddr = AddrOrdinary Addr { addrActorIx = IxAvvm 8 , addrIx = 0 }, outVal = 100000 }
-- >         , Output { outAddr = AddrOrdinary Addr { addrActorIx = IxAvvm 9 , addrIx = 0 }, outVal = 100000 }
-- >         ]
-- >     }]
bootstrapTransactions :: Context -> [DSL.Transaction Addr]
bootstrapTransactions ctxt@Context{..} = [createAda, distributeAda]
  where
    CardanoContext{..} = tcCardano

    -- It's important that we sort this, because the order of the outputs
    -- 'distributeAda' matters. The original list is sorted by 'Address', which
    -- is basically random.
    balances :: [(Addr, Word64)]
    balances =
          sortWith fst
        $ map (bimap (`resolveAddress` ctxt) unsafeGetCoin)
        $ ccBalances

    totalAda :: Word64
    totalAda = sum $ map snd balances -- we're ignoring overflow here

    -- The transaction that generates new Ada coins
    createAda :: DSL.Transaction Addr
    createAda = DSL.Transaction {
          trIns  = []
        , trOuts = [DSL.Output DSL.AddrTreasury totalAda]
        }

    -- The transaction that distributes the new Ada coins
    distributeAda :: DSL.Transaction Addr
    distributeAda = DSL.Transaction {
          trIns  = [DSL.Input createAda 0]
        , trOuts = map (\(addr, val) -> DSL.Output (DSL.AddrOrdinary addr) val)
                       balances
        }

-- | Check if something is the bootstrap transaction
--
-- NOTE: This takes a number of shortcuts so that this is an efficient
-- operation. It will only work correctly when the DSL is used as intended.
isBootstrapTransaction :: DSL.Transaction a -> Bool
isBootstrapTransaction t1 =
    case t1 of
      DSL.Transaction [DSL.Input t0 0] _outs | isCreateAda t0 -> True
      _otherwise -> False
  where
    -- Is this the transaction that created the initial Ada coins?
    isCreateAda :: DSL.Transaction a -> Bool
    isCreateAda (DSL.Transaction [] [DSL.Output DSL.AddrTreasury _val]) = True
    isCreateAda _ = False
