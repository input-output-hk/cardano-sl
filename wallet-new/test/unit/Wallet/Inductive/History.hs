module Wallet.Inductive.History (
    History -- opaque
    -- * Constructing histories from DSL wallets
  , dslInit
  , dslEvent
    -- * Constructing histories for the DSL/core comparison
  , kernelInit
  , kernelEvent
  , kernelInt
  , kernelRollback
  ) where

import           Universum

import           Formatting (bprint)
import qualified Formatting.Buildable
import           Pos.Core.Chrono
import           Serokell.Util (listJson)

import           Cardano.Wallet.Kernel.Util

import           UTxO.Context (Addr)
import           UTxO.DSL (Hash)
import           UTxO.Interpreter (IntCtxt)
import           Wallet.Abstract
import           Wallet.Inductive

{-------------------------------------------------------------------------------
  History
-------------------------------------------------------------------------------}

newtype Event = Event [Text]

-- | History of interpretation (used for debugging)
--
-- We record the history simply as a list of events.
--
-- Implementation note: we store the events in newest-first order so that we
-- can efficiently append elements to the history. We then reverse the history
-- when we pretty-print it.
newtype History = History (NewestFirst [] Event)

{-------------------------------------------------------------------------------
  Constructing histories from pure wallets
-------------------------------------------------------------------------------}

dslInit :: [Wallet h a] -> History
dslInit ws = History $
    NewestFirst [Event ("initial" : map dumpState ws)]

-- | Append an action to a history
dslEvent :: (Hash h a, Buildable a)
         => History          -- ^ Previous history
         -> WalletEvent h a  -- ^ Event to append
         -> [Wallet h a]     -- ^ Wallet states after the event
         -> History
dslEvent (History events) ev ws = History $
    liftNewestFirst (\es -> Event ("state" : map dumpState ws)
                          : Event ["event", pretty ev]
                          : es)
                    events

{-------------------------------------------------------------------------------
  Constructing histories when comparing DSL to kernel
-------------------------------------------------------------------------------}

kernelInit :: Wallet h Addr  -- ^ Initial pure wallet
           -> IntCtxt h      -- ^ Initial interpretation context
           -> History
kernelInit w ic = History $
    NewestFirst [Event ["initial", dumpState w, pretty ic]]

-- | Record that we stepped the wallet
--
-- We do /not/ record the new interpretation context yet, as this is a separate
-- step in the history (see 'kernelInt').
kernelEvent :: Hash h Addr
            => History            -- ^ Previous history
            -> WalletEvent h Addr -- ^ Event to append
            -> Wallet h Addr      -- ^ Pure wallet after the event
            -> History
kernelEvent (History events) ev w = History $
    liftNewestFirst (\es -> Event ["state", dumpState w]
                          : Event ["event", pretty ev]
                          : es)
                    events

-- | Record that we interpreted the event
kernelInt :: History -> IntCtxt h -> History
kernelInt (History events) ic = History $
    liftNewestFirst (\es -> Event ["interpret", pretty ic] : es) events

-- | Record that we rolled back the interpretation state
kernelRollback :: History -> IntCtxt h -> History
kernelRollback (History events) ic = History $
    liftNewestFirst (\es -> Event ["rollback", pretty ic] : es) events

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable Event where
  build (Event e) = bprint listJson e

instance Buildable History where
  build (History events) = bprint listJson (toOldestFirst events)
