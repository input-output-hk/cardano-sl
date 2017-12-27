{-# LANGUAGE TypeFamilies #-}

module Pos.Delegation.RichmenComponent
       ( RCDlg
       , getRichmenDlg
       ) where

import           Universum

import           Pos.Core (BlockVersionData (bvdHeavyDelThd), EpochIndex, HasConfiguration,
                           genesisBlockVersionData)
import           Pos.DB.Class (MonadDBRead)
import           Pos.Lrc.DB.RichmenBase (getRichmen)
import           Pos.Lrc.RichmenComponent (RichmenComponent (..))
import           Pos.Lrc.Types (RichmenSet)
import           Pos.Util.Util (getKeys)


data RCDlg

instance HasConfiguration => RichmenComponent RCDlg where
    type RichmenData RCDlg = RichmenSet
    rcToData = getKeys . snd
    rcTag Proxy = "dlg"
    rcInitialThreshold Proxy = bvdHeavyDelThd genesisBlockVersionData
    rcConsiderDelegated Proxy = False

getRichmenDlg :: MonadDBRead m => EpochIndex -> m (Maybe RichmenSet)
getRichmenDlg epoch = getRichmen @RCDlg epoch
