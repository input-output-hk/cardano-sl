module Wallet.Inductive.History (
    History -- opaque
    -- * "Difference history" (like difference lists)
  , HistoryD -- opaque
  , fromHistoryD
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

-- | History of interpretation
--
-- This is very useful for debugging.
--
-- We record the history simply as a list of events
newtype History = History (OldestFirst [] Event)

{-------------------------------------------------------------------------------
  Difference lists
-------------------------------------------------------------------------------}

-- | "Difference history"
newtype HistoryD = HistoryD (NewestFirst [] Event)

fromHistoryD :: HistoryD -> History
fromHistoryD (HistoryD events) = History (toOldestFirst events)

{-------------------------------------------------------------------------------
  Constructing histories from pure wallets
-------------------------------------------------------------------------------}

dslInit :: [Wallet h a] -> HistoryD
dslInit ws = HistoryD $
    NewestFirst [Event ("initial" : map dumpState ws)]

-- | Append an action to a history
dslEvent :: (Hash h a, Buildable a)
         => HistoryD         -- ^ Previous history
         -> WalletEvent h a  -- ^ Event to append
         -> [Wallet h a]     -- ^ Wallet states after the event
         -> HistoryD
dslEvent (HistoryD events) ev ws = HistoryD $
    liftNewestFirst (\es -> Event ("state" : map dumpState ws)
                          : Event ["event", pretty ev]
                          : es)
                    events

{-------------------------------------------------------------------------------
  Constructing histories when comparing DSL to kernel
-------------------------------------------------------------------------------}

kernelInit :: Hash h Addr
           => Wallet h Addr  -- ^ Initial pure wallet
           -> IntCtxt h      -- ^ Initial interpretation context
           -> HistoryD
kernelInit w ic = HistoryD $
    NewestFirst [Event ["initial", "<wallet>" {-dumpState w -}, pretty ic]]

-- | Record that we stepped the wallet
--
-- We do /not/ record the new interpretation context yet, as this is a separate
-- step in the history (see 'kernelInt').
kernelEvent :: Hash h Addr
            => HistoryD           -- ^ Previous history
            -> WalletEvent h Addr -- ^ Event to append
            -> Wallet h Addr      -- ^ Pure wallet after the event
            -> HistoryD
kernelEvent (HistoryD events) ev w = HistoryD $
    liftNewestFirst (\es -> Event ["state", "<wallet>" {- dumpState w -}]
                          : Event ["event", pretty ev]
                          : es)
                    events

-- | Record that we interpreted the event
kernelInt :: Hash h Addr => HistoryD -> IntCtxt h -> HistoryD
kernelInt (HistoryD events) ic = HistoryD $
    liftNewestFirst (\es -> Event ["interpret", pretty ic] : es) events

-- | Record that we rolled back the interpretation state
kernelRollback :: Hash h Addr => HistoryD -> IntCtxt h -> HistoryD
kernelRollback (HistoryD events) ic = HistoryD $
    liftNewestFirst (\es -> Event ["rollback", pretty ic] : es) events

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable Event where
  build (Event e) = bprint listJson e

instance Buildable History where
  build (History events) = bprint listJson events
