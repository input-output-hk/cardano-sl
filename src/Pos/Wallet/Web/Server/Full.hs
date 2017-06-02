-- | Re-exports of Pos.Wallet.Web.Server.Full.* functionality.
--
-- Initially they were all collected in single module, but functions like
-- `walletServeWebFull` consume too much memory and we can't afford keeping
-- many such methods together.

{-# OPTIONS_GHC -F -pgmF autoexporter #-}
