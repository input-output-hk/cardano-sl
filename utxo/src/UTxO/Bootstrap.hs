-- | Generate bootstrap transactions for the DSL
module UTxO.Bootstrap (
    bootstrapTransaction
  , isBootstrapTransaction
  ) where

import           Data.Bifunctor (bimap)
import qualified Data.Set as Set
import           Universum

import           Pos.Core

import           UTxO.Context
import qualified UTxO.DSL as DSL

-- | Construct the bootstrap transaction
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
-- In order to be able to compare the DSL or Cardano, we need to create an
-- initial "bootstrap" transaction that sets things up to mirror the Cardano
-- genesis block. For the specific context generated in the test genesis block,
-- we generate
--
-- > Transaction{
-- >     fresh: 44999999999999992
-- >   , ins:   []
-- >   , outs:  [
-- >       Output{ addr: Addr{ actorIx: IxRich 0, addrIx:  0}, val:  11137499999752500}
-- >     , Output{ addr: Addr{ actorIx: IxRich 1, addrIx:  0}, val:  11137499999752500}
-- >     , Output{ addr: Addr{ actorIx: IxRich 2, addrIx:  0}, val:  11137499999752500}
-- >     , Output{ addr: Addr{ actorIx: IxRich 3, addrIx:  0}, val:  11137499999752500}
-- >     , Output{ addr: Addr{ actorIx: IxPoor 0, addrIx:  0}, val:  37499999999166}
-- >     , Output{ addr: Addr{ actorIx: IxPoor 1, addrIx:  0}, val:  37499999999166}
-- >     , Output{ addr: Addr{ actorIx: IxPoor 2, addrIx:  0}, val:  37499999999166}
-- >     , Output{ addr: Addr{ actorIx: IxPoor 3, addrIx:  0}, val:  37499999999166}
-- >     , Output{ addr: Addr{ actorIx: IxPoor 4, addrIx:  0}, val:  37499999999166}
-- >     , Output{ addr: Addr{ actorIx: IxPoor 5, addrIx:  0}, val:  37499999999166}
-- >     , Output{ addr: Addr{ actorIx: IxPoor 6, addrIx:  0}, val:  37499999999166}
-- >     , Output{ addr: Addr{ actorIx: IxPoor 7, addrIx:  0}, val:  37499999999166}
-- >     , Output{ addr: Addr{ actorIx: IxPoor 8, addrIx:  0}, val:  37499999999166}
-- >     , Output{ addr: Addr{ actorIx: IxPoor 9, addrIx:  0}, val:  37499999999166}
-- >     , Output{ addr: Addr{ actorIx: IxPoor 10, addrIx:  0}, val:  37499999999166}
-- >     , Output{ addr: Addr{ actorIx: IxPoor 11, addrIx:  0}, val:  37499999999166}
-- >     , Output{ addr: Addr{ actorIx: IxAvvm 0, addrIx:  0}, val:  100000}
-- >     , Output{ addr: Addr{ actorIx: IxAvvm 1, addrIx:  0}, val:  100000}
-- >     , Output{ addr: Addr{ actorIx: IxAvvm 2, addrIx:  0}, val:  100000}
-- >     , Output{ addr: Addr{ actorIx: IxAvvm 3, addrIx:  0}, val:  100000}
-- >     , Output{ addr: Addr{ actorIx: IxAvvm 4, addrIx:  0}, val:  100000}
-- >     , Output{ addr: Addr{ actorIx: IxAvvm 5, addrIx:  0}, val:  100000}
-- >     , Output{ addr: Addr{ actorIx: IxAvvm 6, addrIx:  0}, val:  100000}
-- >     , Output{ addr: Addr{ actorIx: IxAvvm 7, addrIx:  0}, val:  100000}
-- >     , Output{ addr: Addr{ actorIx: IxAvvm 8, addrIx:  0}, val:  100000}
-- >     , Output{ addr: Addr{ actorIx: IxAvvm 9, addrIx:  0}, val:  100000}
-- >   ]
-- >   , fee:   0
-- >   , hash:  0
-- > }
bootstrapTransaction :: TransCtxt -> DSL.Transaction h Addr
bootstrapTransaction ctxt@TransCtxt{..} = DSL.Transaction {
      trFresh = totalAda
    , trIns   = Set.empty
    , trOuts  = map (uncurry DSL.Output) balances
    , trFee   = 0
    , trHash  = 0
    , trExtra = ["Bootstrap transaction"]
    }
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

-- | Check if something is the bootstrap transaction
isBootstrapTransaction :: DSL.Transaction h a -> Bool
isBootstrapTransaction = Set.null . DSL.trIns
