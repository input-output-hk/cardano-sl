{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}

module Dbgen.QueryMethods where

import           Universum

import           Pos.Wallet.Web.Methods.Logic (getWallets)
import           Text.Printf (printf)

import           Dbgen.Lib (timed)
import           Dbgen.Rendering (say)
import           Dbgen.Types (Method (..), UberMonad)

queryMethods :: Maybe Method -> UberMonad ()
queryMethods Nothing = say "No valid method read from the CLI."
queryMethods (Just method) = case method of
  GetWallets -> queryGetWallets


queryGetWallets :: UberMonad ()
queryGetWallets = do
  wallets <- timed getWallets
  case wallets of
    [] -> say "No wallets returned."
    _  -> say $ printf "%d wallets found." (length wallets)

