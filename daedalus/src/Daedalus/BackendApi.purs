module Daedalus.BackendApi where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error, Error)
import Control.Monad.Error.Class (throwError)
import Daedalus.Constants (backendPrefix)
import Daedalus.Types (CAddress, _address, _ccoin, CWallet, CTx, CWalletMeta, CTxId, CTxMeta, _ctxIdValue, CCurrency, WalletError, showCCurrency, CProfile, CWalletInit, CUpdateInfo, SoftwareVersion, CWalletRedeem, SyncProgress, CInitialized, CPassPhrase, _passPhrase, CCoin, CPaperVendWalletRedeem)
import Data.Array (last)
import Data.Argonaut (Json)
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Bifunctor (bimap)
import Data.Either (either, Either(Left))
import Data.Generic (class Generic, gShow)
import Data.HTTP.Method (Method(POST, PUT, DELETE))
import Data.Maybe (Maybe(Just))
import Data.MediaType.Common (applicationJSON)
import Data.String (joinWith)
import Data.Tuple (Tuple)
import Network.HTTP.Affjax (AffjaxResponse, affjax, defaultRequest, AJAX, URL, AffjaxRequest)
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.RequestHeader (RequestHeader(ContentType))
import Network.HTTP.StatusCode (StatusCode(..))

-- HELPERS

type URLPath = Array String

mkUrl :: URLPath -> URL
mkUrl = joinWith "/"

backendApi :: URLPath -> URL
backendApi path = mkUrl $ [backendPrefix, "api"] <> path <> ifEmptyEnd
  where
    -- Workaround for passing empty passphrases as last capture in URL
    ifEmptyEnd = if last path == Just "" then [""] else []

data ApiError
    = HTTPStatusError (AffjaxResponse Json)
    | JSONDecodingError String
    | ServerError WalletError

instance showApiError :: Show ApiError where
    show (HTTPStatusError res) =
        "HTTPStatusError: " <> show res.status <> " msg: " <> show res.response
    show (JSONDecodingError e) =
        "JSONDecodingError: " <> show e
    show (ServerError e) =
        "ServerError: " <> gShow e

-- REQUESTS HELPERS

decodeResult :: forall a eff. Generic a => {response :: Json | eff} -> Either Error a
decodeResult = either (Left <<< mkJSONError) (bimap mkServerError id) <<< decodeJson <<< _.response
  where
    mkJSONError = error <<< show <<< JSONDecodingError
    mkServerError = error <<< show <<< ServerError

makeRequest :: forall eff a r. (Generic a, Requestable r) => AffjaxRequest r -> URLPath -> Aff (ajax :: AJAX | eff) a
makeRequest request urlPath = do
    res <- affjax $ request { url = backendApi urlPath }
    when (isHttpError res.status) $
        throwError <<< error <<< show $ HTTPStatusError res
    either throwError pure $ decodeResult res
  where
    isHttpError (StatusCode c) = c >= 400

getR :: forall eff a. Generic a => URLPath -> Aff (ajax :: AJAX | eff) a
getR = makeRequest defaultRequest

postR :: forall eff a. Generic a => URLPath -> Aff (ajax :: AJAX | eff) a
postR = makeRequest $ defaultRequest { method = Left POST }

postRBody :: forall eff a b. (Generic a, Generic b) => URLPath -> b -> Aff (ajax :: AJAX | eff) a
postRBody urlPath content = flip makeRequest urlPath $
    defaultRequest { method = Left POST
                   , content = Just <<< show $ encodeJson content
                   , headers = [ContentType applicationJSON]
                   }

putRBody :: forall eff a b. (Generic a, Generic b) => URLPath -> b -> Aff (ajax :: AJAX | eff) a
putRBody urlPath content = flip makeRequest urlPath $
    defaultRequest { method = Left PUT
                   , content = Just <<< show $ encodeJson content
                   , headers = [ContentType applicationJSON]
                   }

deleteR :: forall eff a. Generic a => URLPath -> Aff (ajax :: AJAX | eff) a
deleteR = makeRequest $ defaultRequest { method = Left DELETE }

-- REQUESTS
--------------------------------------------------------------------------------
-- TEST ------------------------------------------------------------------------
testReset :: forall eff. Aff (ajax :: AJAX | eff) Unit
testReset = postR ["test", "reset"]
--------------------------------------------------------------------------------
-- WALLETS ---------------------------------------------------------------------
getWallets :: forall eff. Aff (ajax :: AJAX | eff) (Array CWallet)
getWallets = getR ["wallets"]

getWallet :: forall eff. CAddress -> Aff (ajax :: AJAX | eff) CWallet
getWallet addr = getR ["wallets", _address addr]

updateWallet :: forall eff. CAddress -> CWalletMeta -> Aff (ajax :: AJAX | eff) CWallet
updateWallet addr = putRBody ["wallets", _address addr]

newWallet :: forall eff. CPassPhrase -> CWalletInit -> Aff (ajax :: AJAX | eff) CWallet
newWallet pass = postRBody ["wallets", _passPhrase pass]

deleteWallet :: forall eff. CAddress -> Aff (ajax :: AJAX | eff) Unit
deleteWallet addr = deleteR ["wallets", _address addr]

importKey :: forall eff. String -> Aff (ajax :: AJAX | eff) CWallet
importKey = postRBody ["wallets", "keys"]

restoreWallet :: forall eff. CPassPhrase -> CWalletInit -> Aff (ajax :: AJAX | eff) CWallet
restoreWallet pass = postRBody ["wallets", "restore", _passPhrase pass]
--------------------------------------------------------------------------------
-- ADDRESSSES ------------------------------------------------------------------
isValidAddress :: forall eff. CCurrency -> String -> Aff (ajax :: AJAX | eff) Boolean
isValidAddress cCurrency addr = getR ["addresses", addr, "currencies", showCCurrency cCurrency]
--------------------------------------------------------------------------------
-- PROFILES --------------------------------------------------------------------
getProfile :: forall eff. Aff (ajax :: AJAX | eff) CProfile
getProfile = getR ["profile"]

updateProfile :: forall eff. CProfile -> Aff (ajax :: AJAX | eff) CProfile
updateProfile = postRBody ["profile"]
--------------------------------------------------------------------------------
-- TRANSACTIONS ----------------------------------------------------------------
send :: forall eff. CPassPhrase -> CAddress -> CAddress -> CCoin -> Aff (ajax :: AJAX | eff) CTx
send pass addrFrom addrTo amount = postR ["txs", "payments", _passPhrase pass, _address addrFrom, _address addrTo, _ccoin amount]

sendExtended :: forall eff. CPassPhrase -> CAddress -> CAddress -> CCoin -> CCurrency -> String -> String -> Aff (ajax :: AJAX | eff) CTx
sendExtended pass addrFrom addrTo amount curr title desc = postR ["txs", "payments", _passPhrase pass, _address addrFrom, _address addrTo, _ccoin amount, showCCurrency curr, title, desc]

updateTransaction :: forall eff. CAddress -> CTxId -> CTxMeta -> Aff (ajax :: AJAX | eff) Unit
updateTransaction addr ctxId = postRBody ["txs", "payments", _address addr, _ctxIdValue ctxId]

getHistory :: forall eff. CAddress -> Int -> Int -> Aff (ajax :: AJAX | eff) (Tuple (Array CTx) Int)
getHistory addr skip limit = getR ["txs", "histories", _address addr <> "?skip=" <> show skip <> "&limit=" <> show limit]

searchHistory :: forall eff. CAddress -> String -> Int -> Int -> Aff (ajax :: AJAX | eff) (Tuple (Array CTx) Int)
searchHistory addr search skip limit = getR ["txs", "histories", _address addr, search <> "?skip=" <> show skip <> "&limit=" <> show limit]
--------------------------------------------------------------------------------
-- UPDATES ---------------------------------------------------------------------
nextUpdate :: forall eff. Aff (ajax :: AJAX | eff) CUpdateInfo
nextUpdate = getR ["update"]

applyUpdate :: forall eff. Aff (ajax :: AJAX | eff) Unit
applyUpdate = postR ["update"]
--------------------------------------------------------------------------------
-- REDEMPTIONS -----------------------------------------------------------------
redeemAda :: forall eff. CWalletRedeem -> Aff (ajax :: AJAX | eff) CTx
redeemAda = postRBody ["redemptions", "ada"]

redeemAdaPaperVend :: forall eff. CPaperVendWalletRedeem -> Aff (ajax :: AJAX | eff) CTx
redeemAdaPaperVend = postRBody ["papervend", "redemptions", "ada"]
--------------------------------------------------------------------------------
-- REPORTING ---------------------------------------------------------------------
reportInit :: forall eff. CInitialized -> Aff (ajax :: AJAX | eff) Unit
reportInit = postRBody ["reporting", "initialized"]
--------------------------------------------------------------------------------
-- SETTINGS ---------------------------------------------------------------------
blockchainSlotDuration :: forall eff. Aff (ajax :: AJAX | eff) Int
blockchainSlotDuration = getR ["settings", "slots", "duration"]

systemVersion :: forall eff. Aff (ajax :: AJAX | eff) SoftwareVersion
systemVersion = getR ["settings", "version"]

syncProgress :: forall eff. Aff (ajax :: AJAX | eff) SyncProgress
syncProgress = getR ["settings", "sync", "progress"]
--------------------------------------------------------------------------------
