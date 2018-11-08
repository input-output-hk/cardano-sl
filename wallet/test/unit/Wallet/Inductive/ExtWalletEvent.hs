module Wallet.Inductive.ExtWalletEvent (
    ExtWalletEvent(..)
  , UseWalletWorker(..)
  , extWalletEvents
  ) where

import           Universum

import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable
import           Serokell.Util (listJson)

import           Pos.Core.Chrono

import qualified Cardano.Wallet.Kernel.Actions as WW

import           UTxO.DSL (Hash)
import qualified UTxO.DSL as DSL
import           Wallet.Inductive

{-------------------------------------------------------------------------------
  Extended events
-------------------------------------------------------------------------------}

-- | 'WalletEvent' extended with 'SwitchToFork'
data ExtWalletEvent h a =
    -- | Inform the wallet of a new block added to the blockchain
    ExtApplyBlock (DSL.Block h a)

    -- | Submit a new transaction to the wallet to be included in the blockchain
  | ExtNewPending (DSL.Transaction h a)

    -- | Roll back the last block added to the blockchain
  | ExtRollback

    -- | Switch to fork
  | ExtSwitchToFork Int (OldestFirst NE (DSL.Block h a))

-- | Inject 'WalletEvent' into 'ExtWalletEvent' without using 'ExtSwitchToFork'
extSimple :: OldestFirst [] (WalletEvent h a)
          -> OldestFirst [] (ExtWalletEvent h a)
extSimple = fmap aux
  where
    aux :: WalletEvent h a -> ExtWalletEvent h a
    aux (ApplyBlock b) = ExtApplyBlock b
    aux (NewPending t) = ExtNewPending t
    aux Rollback       = ExtRollback

-- | Translate from 'WalletEvent' to 'ExtWalletEvent' using the wallet worker
extUsingWalletWorker :: forall h a.
                        OldestFirst [] (WalletEvent h a)
                     -> OldestFirst [] (ExtWalletEvent h a)
extUsingWalletWorker (OldestFirst events) = OldestFirst $
    go WW.initialWorkerState events
  where
    go :: WW.WalletWorkerState (DSL.Block h a)
       -> [WalletEvent h a] -> [ExtWalletEvent h a]
    go _ww []     = []
    go  ww (e:es) =
      case e of
        ApplyBlock b ->
          let (ww', acts) = WW.interpStep (toAction (Left b)) ww
          in concatMap fromAction acts ++ go ww' es
        NewPending t ->
          ExtNewPending t : go ww es
        Rollback ->
          let (ww', acts) = WW.interpStep (toAction (Right ())) ww
          in concatMap fromAction acts ++ go ww' es

    -- Return apply block or rollback into wallet action
    toAction :: Either (DSL.Block h a) () -> WW.WalletAction (DSL.Block h a)
    toAction (Left b)   = WW.ApplyBlocks $ OldestFirst (b :| [])
    toAction (Right ()) = WW.RollbackBlocks 1

    fromAction :: WW.WalletInterpAction (DSL.Block h a) -> [ExtWalletEvent h a]
    fromAction (WW.InterpApplyBlocks bs)    = map ExtApplyBlock (toList bs)
    fromAction (WW.InterpSwitchToFork n bs) = [ExtSwitchToFork n bs]
    fromAction (WW.InterpLogMessage _)      = []

-- | Should we use the wallet worker during interpretation?
data UseWalletWorker =
    -- | Use wallet worker: collapse @rollback@/@applyBlock@ to @switchToFork@
    UseWalletWorker

    -- | Don't use the wallet worker: apply rollback directly
  | DontUseWalletWorker

extWalletEvents :: UseWalletWorker
                -> OldestFirst [] (WalletEvent h a)
                -> OldestFirst [] (ExtWalletEvent h a)
extWalletEvents UseWalletWorker     = extUsingWalletWorker
extWalletEvents DontUseWalletWorker = extSimple

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance (Hash h a, Buildable a) => Buildable (OldestFirst [] (ExtWalletEvent h a)) where
  build = bprint listJson . getOldestFirst

instance (Hash h a, Buildable a) => Buildable (ExtWalletEvent h a) where
  build (ExtApplyBlock b)      = bprint ("ExtApplyBlock " % build) b
  build (ExtNewPending t)      = bprint ("ExtNewPending " % build) t
  build (ExtSwitchToFork n bs) = bprint ("ExtSwitchToFork " % build % " " % listJson) n bs
  build ExtRollback            = bprint "ExtRollback"
