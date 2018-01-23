-- | TODO: real values for the client function.

module Client.Pos.Wallet.Web.Endpoint.NewPayment
    ( newPaymentIO
    ) where

import           Universum

import           Client.Pos.Wallet.Web.Api  (newPayment)
import           Client.Pos.Wallet.Web.Run  (runEndpointClient)

import           Pos.Core.Types             (mkCoin)
import           Pos.Wallet.Web.ClientTypes (CAccountId (..),
                                             CId (..), CHash (..)) -- , CPassPhrase, CTx)

-- | Run 'NewPayment' client. As a result we get a newly created transaction.
newPaymentIO :: IO ()
newPaymentIO =
    let passPhrase = Nothing        -- :: Maybe CPassPhrase
        accountId  = CAccountId ""  -- :: CAccountId
        address    = CId (CHash "") -- :: CId Addr
        coin       = mkCoin 100     -- :: Coin
        policy     = Nothing        -- :: Maybe InputSelectionPolicy
    in
    runEndpointClient (newPayment passPhrase accountId address coin policy) >>= \case
        Left problem -> putText $ "Cannot create new payment: " <> problem
        Right newTx  -> print newTx -- :: CTx

{-
data CTx = CTx
    { ctId            :: CTxId
    , ctAmount        :: CCoin
    , ctConfirmations :: Word
    , ctMeta          :: CTxMeta
    , ctInputs        :: [(CId Addr, CCoin)]
    , ctOutputs       :: [(CId Addr, CCoin)]
    , ctIsLocal       :: Bool
    , ctIsOutgoing    :: Bool
    , ctCondition     :: CPtxCondition
    } deriving (Show, Generic, Typeable)
-}
