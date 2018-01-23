-- | Generate bootstrap transactions for the DSL
module UTxO.Bootstrap (
    bootstrapTransaction
  , isBootstrapTransaction
  , bootstrapBeneficiary
  ) where

import Universum
import Data.List ((!!))

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
-- set up the initial balances for all actors, but if we did  that we'd need a
-- whole chain of transactions to distribute the balances.)
--
-- (Note on "redeem addresses": these are only relevant for AVVM accounts,
-- which we ignore completely in this setup at the moment.)
bootstrapTransaction :: Context -> DSL.Transaction Addr
bootstrapTransaction ctxt@Context{..} = distributeAda
  where
    CardanoContext{..} = tcCardano

    knownBalances :: [(Addr, Word64)]
    knownBalances =
          -- It's important that we sort this, because the order
          -- of the outputs 'distributeAda' matters. The original
          -- was sorted by 'Address', which is basically random.
          sortBy (comparing fst)
          -- Filter out AVVM addresses, and translate from
          -- Cardano addresses to DSL addresses
        $ mapMaybe (\(addr, coin) -> do
                      guard $ isKnownAddress addr ctxt
                      return ( resolveAddress addr ctxt
                             , unsafeGetCoin coin)
                             )
        $ ccBalances

    totalAda :: Word64
    totalAda = sum $ map snd knownBalances -- we're ignoring overflow here

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
                       knownBalances
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

-- | The beneficiary of a particular output of the bootstrap transaction
--
-- PRE: Input must be the bootstrap transaction (see 'isBootstrapTransaction').
bootstrapBeneficiary :: DSL.Transaction Addr -> DSL.Index -> Addr
bootstrapBeneficiary t1 ix = addr
  where
    DSL.Transaction _in outs             = t1
    DSL.Output (DSL.AddrOrdinary addr) _ = outs !! fromIntegral ix
