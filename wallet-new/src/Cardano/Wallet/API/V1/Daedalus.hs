-- | This module contains the top level API definition for frontend-related
-- tasks.  The API endpoints presented here are intended for use with the
-- Daedalus client, and aren't useful for wallets, exchanges, and other users.
module Cardano.Wallet.API.V1.Daedalus where

-- migrate everything except for import/export

type API =
    "daedalus"
        :> ( "update"
            :> ( "apply" :> Get '[ValidJSON] (WalletResponse ())
            :<|> "postpone" :> Get '[ValidJSON] (WalletResponse ())
            )
        :<|> "papervend" :>  "redemptions" :> "ada" :> Get '[ValidJSON] (WalletResponse ())
        :<|> "redemptions" :> "ada" :> Get '[ValidJSON] (WalletResponse ())
        )

-- redeemAda
-- ProtocolMagic -> (TxAux -> m Bool) -> PassPhrase -> CWalletRedeem -> m CTx
--
-- -- | Query data for redeem
-- data CWalletRedeem = CWalletRedeem
--     { crWalletId :: !CAccountId
--     , crSeed     :: !Text -- TODO: newtype!
--     } deriving (Show, Generic)

-- redeemAdaPaperVend
--     :: MonadWalletTxFull ctx m
--     => ProtocolMagic
--     -> (TxAux -> m Bool)
--     -> PassPhrase
--     -> CPaperVendWalletRedeem
--     -> m CTx
-- data CPaperVendWalletRedeem = CPaperVendWalletRedeem
--     { pvWalletId     :: !CAccountId
--     , pvSeed         :: !Text -- TODO: newtype!
--     , pvBackupPhrase :: !(CBackupPhrase 9)
--     } deriving (Show, Generic)


-- update postpone
-- -- | Postpone next update after restart
-- postponeUpdate :: (MonadIO m, WalletDbReader ctx m) => m NoContent
-- postponeUpdate = askWalletDB >>= removeNextUpdate >> return NoContent
--

-- update apply
--
-- -- | Delete next update info and restart immediately
-- applyUpdate :: ( MonadIO m
--                , WalletDbReader ctx m
--                , MonadUpdates m
--                )
--             => m NoContent
-- applyUpdate = askWalletDB >>= removeNextUpdate
--               >> applyLastUpdate >> return NoContent
