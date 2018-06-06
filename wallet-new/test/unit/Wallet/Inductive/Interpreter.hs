module Wallet.Inductive.Interpreter (
    -- * History
    History(..)
  , HistoryD
  , fromHistoryD
  , historyD
  , snocHistoryD
    -- * Interpreter proper
  , interpret
  , InvalidInput(..)
  ) where

import           Universum

import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))
import           Pos.Core.Chrono
import           Serokell.Util (listJson)

import           Util.Validated
import           UTxO.DSL
import           Wallet.Abstract
import           Wallet.Inductive

{-------------------------------------------------------------------------------
  History
-------------------------------------------------------------------------------}

-- | History of interpretation
--
-- This is very useful for debugging
data History h a = History {
      -- | Wallet internal states
      historyState :: [Text]

      -- | Continuation of the history (if any)
    , historyStep :: Maybe (WalletEvent h a, History h a)
    }

-- | "Difference history"
type HistoryD h a = Maybe (WalletEvent h a, History h a) -> History h a

fromHistoryD :: HistoryD h a -> History h a
fromHistoryD = ($ Nothing)

historyD :: [Wallet h a] -> HistoryD h a
historyD = History . map dumpState

-- | Append an action to a history
snocHistoryD :: HistoryD h a     -- ^ Previous history
             -> WalletEvent h a  -- ^ Event to append
             -> [Wallet h a]     -- ^ Wallet states after the event
             -> HistoryD h a
snocHistoryD k ev ws h = k $ Just (ev, historyD ws h)

{-------------------------------------------------------------------------------
  Interpreter
-------------------------------------------------------------------------------}

-- | Interpreter for 'Inductive'
--
-- Given (one or more) wallet constructors, evaluate an 'Inductive' wallet,
-- checking the given property at each step.
--
-- Note: we expect the 'Inductive' to be valid (valid blockchain, valid
-- calls to 'newPending', etc.). This is meant to check properties of the
-- /wallet/, not the wallet input. See 'isInductiveValid'.
interpret :: forall h a err.
             (History h a -> InvalidInput h a -> err)
          -- ^ Inject invalid input err.
          -- We provide the events that lead to the error.
          -> (Transaction h a -> [Wallet h a])
          -- ^ Wallet constructors
          -> (History h a -> [Wallet h a] -> Validated err ())
          -- ^ Predicate to check. The predicate is passed the events leading
          -- to this point, for better error messages.
          -> Inductive h a
          -- ^ 'Inductive' value to interpret
          -> Validated err [Wallet h a]
interpret invalidInput mkWallets p Inductive{..} =
    goBoot inductiveBoot
  where
    goBoot :: Transaction h a -> Validated err [Wallet h a]
    goBoot boot = do
        let acc'     = mkWallets boot
            history' = historyD acc'
        verify history' acc'
        goEvents history' acc' (getOldestFirst inductiveEvents)

    goEvents :: HistoryD h a       -- history
             -> [Wallet h a]       -- accumulator
             -> [WalletEvent h a]  -- events to process
             -> Validated err [Wallet h a]
    goEvents _ acc [] =
        return acc
    goEvents history acc (ApplyBlock b:es) = do
        let acc'     = map (`applyBlock` b) acc
            history' = snocHistoryD history (ApplyBlock b) acc'
        verify history' acc'
        goEvents history' acc' es
    goEvents history acc (NewPending t:es) = do
        acc' <- mapM (newPending' history t) acc
        let history' = snocHistoryD history (NewPending t) acc'
        verify history' acc'
        goEvents history' acc' es
    goEvents history acc (Rollback:es) = do
        let acc'     = map rollback acc
            history' = snocHistoryD history Rollback acc'
        verify history' acc'
        goEvents history' acc' es

    verify :: HistoryD h a
           -> [Wallet h a] -> Validated err ()
    verify history ws = p (fromHistoryD history) ws

    newPending' :: HistoryD h a
                -> Transaction h a
                -> Wallet h a -> Validated err (Wallet h a)
    newPending' history tx w =
        case newPending w tx of
          Just w' -> return w'
          Nothing -> throwError . invalidInput (fromHistoryD history)
                   $ InvalidPending tx (utxo w) (pending w)

-- | We were unable to check the invariant because the input was invalid
--
-- This indicates a bug in the generator (or in the hand-written 'Inductive'),
-- so we try to provide sufficient information to track that down.
data InvalidInput h a =
    InvalidPending {
        -- | The submitted transaction that was invalid
        invalidPendingTransaction   :: Transaction h a

        -- | The UTxO of the wallet at the time of submission
      , invalidPendingWalletUtxo    :: Utxo h a

        -- | The pending set of the wallet at time of submission
      , invalidPendingWalletPending :: Pending h a
      }

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance (Hash h a, Buildable a) => Buildable (InvalidInput h a) where
  build InvalidPending{..} = bprint
    ( "InvalidPending "
    % "{ transaction:   " % build
    % ", walletUtxo:    " % build
    % ", walletPending: " % build
    % "}"
    )
    invalidPendingTransaction
    invalidPendingWalletUtxo
    invalidPendingWalletPending

instance (Hash h a, Buildable a) => Buildable (History h a) where
  build = bprint ("{" % build) . go
    where
      go (History s n)  = bprint ("state: " % listJson % build) s (go' n)

      go' Nothing       = "}"
      go' (Just (e, h)) = bprint (", action: " % build % ", " % build) e (go h)
