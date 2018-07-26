module Wallet.Inductive.Interpreter (
    interpret
  , InvalidInput(..)
  ) where

import           Universum

import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable
import           Pos.Core.Chrono

import           Util.Validated
import           UTxO.DSL
import           Wallet.Abstract
import           Wallet.Inductive
import           Wallet.Inductive.History

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
interpret :: forall h a err. (Hash h a, Buildable a)
          => (History -> InvalidInput h a -> err)
          -- ^ Inject invalid input err.
          -- We provide the events that lead to the error.
          -> (Transaction h a -> [Wallet h a])
          -- ^ Wallet constructors
          -> (History -> [Wallet h a] -> Validated err ())
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
            history' = dslInit acc'
        verify history' acc'
        goEvents history' acc' (getOldestFirst inductiveEvents)

    goEvents :: History            -- history
             -> [Wallet h a]       -- accumulator
             -> [WalletEvent h a]  -- events to process
             -> Validated err [Wallet h a]
    goEvents _ acc [] =
        return acc
    goEvents history acc (ApplyBlock b:es) = do
        let acc'     = map (`applyBlock` b) acc
            history' = dslEvent history (ApplyBlock b) acc'
        verify history' acc'
        goEvents history' acc' es
    goEvents history acc (NewPending t:es) = do
        acc' <- mapM (newPending' history t) acc
        let history' = dslEvent history (NewPending t) acc'
        verify history' acc'
        goEvents history' acc' es
    goEvents history acc (Rollback:es) = do
        let acc'     = map rollback acc
            history' = dslEvent history Rollback acc'
        verify history' acc'
        goEvents history' acc' es

    verify :: History -> [Wallet h a] -> Validated err ()
    verify history ws = p history ws

    newPending' :: History
                -> Transaction h a
                -> Wallet h a -> Validated err (Wallet h a)
    newPending' history tx w =
        case newPending w tx of
          Just w' -> return w'
          Nothing -> throwError . invalidInput history
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
