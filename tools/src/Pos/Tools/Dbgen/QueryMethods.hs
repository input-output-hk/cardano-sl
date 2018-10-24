{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}

module Pos.Tools.Dbgen.QueryMethods where

import           Universum

import           Pos.Core.NetworkMagic (NetworkMagic)
import           Pos.Wallet.Web.Methods.Logic (getWallets)
import           Text.Printf (printf)

import           Pos.Tools.Dbgen.Lib (timed)
import           Pos.Tools.Dbgen.Rendering (say)
import           Pos.Tools.Dbgen.Types (Method (..), UberMonad)

queryMethods :: NetworkMagic -> Maybe Method -> UberMonad ()
queryMethods _  Nothing = say "No valid method read from the CLI."
queryMethods nm (Just method) = case method of
  GetWallets -> queryGetWallets nm


queryGetWallets :: NetworkMagic -> UberMonad ()
queryGetWallets nm = do
  wallets <- timed (getWallets nm)
  case wallets of
    [] -> say "No wallets returned."
    _  -> say $ printf "%d wallets found." (length wallets)

