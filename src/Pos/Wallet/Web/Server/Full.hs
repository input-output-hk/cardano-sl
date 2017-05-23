-- | Re-exports of Pos.Wallet.Web.Server.Full.* functionality.
--
-- Initially it was all collected in single module, but functions like
-- `walletServeWebFull` consume too much memory and we can't afford keeping
-- two such methods together.

{-# OPTIONS_GHC -F -pgmF autoexporter #-}
