{-# LANGUAGE FlexibleContexts #-}

module Pos.Security
       ( module Pos.Security.Workers
       , shouldIgnoreAddress
       ) where

import           Control.TimeWarp.Rpc (NetworkAddress)
import           Universum

import           Pos.Context          (getNodeContext, ncMalicious)
import           Pos.Security.Workers
import           Pos.WorkMode         (WorkMode)

shouldIgnoreAddress :: WorkMode scc m => NetworkAddress -> m Bool
shouldIgnoreAddress addr = elem addr <$> ncMalicious <$> getNodeContext
