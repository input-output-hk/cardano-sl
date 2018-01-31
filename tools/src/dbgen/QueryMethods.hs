{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module QueryMethods where

import           Universum

import           Pos.Wallet.Web.Methods.Logic (getWallets)
import           Text.Printf (printf)

import           Lib (timed)
import           Rendering (say)
import           Types (Method (..), UberMonad)

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

