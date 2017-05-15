module Daedalus.BackendApi where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Exception (error, Error, throwException)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Error.Class (throwError)
import Daedalus.Constants (backendPrefix)
import Daedalus.Types (CAddress, _address, _ccoin, CWallet, CTx, CWalletMeta, CTxId, CTxMeta, _ctxIdValue, CCurrency, WalletError, showCCurrency, CProfile, CWalletInit, CUpdateInfo, SoftwareVersion, CWalletRedeem, SyncProgress, CInitialized, CPassPhrase, _passPhrase, CCoin, CPaperVendWalletRedeem)
import Data.Array (last)
import Data.Argonaut (Json)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Bifunctor (bimap, lmap)
import Data.Either (either, Either(Left))
import Data.Generic (class Generic, gShow)
import Data.HTTP.Method (Method(GET, POST, PUT, DELETE))
import Data.Maybe (Maybe(Just))
import Data.MediaType.Common (applicationJSON)
import Data.String (joinWith)
import Data.Tuple (Tuple (..))
import Data.StrMap (fromFoldable)
import Daedalus.TLS (TLSOptions)
import Node.HTTP.Client (method, path, request, statusCode, statusMessage, Response, responseAsStream, headers, RequestHeaders (..), Request, requestAsStream)
import Data.Options ((:=))
import Node.Encoding (Encoding (UTF8))
import Node.Stream (onDataString, writeString, end)
import Node.HTTP (HTTP)

-- HELPERS

type URLPath = Array String
type URL = String

mkUrl :: URLPath -> URL
mkUrl = joinWith "/"

backendApi :: URLPath -> URL
backendApi path = mkUrl $ [backendPrefix, "api"] <> path <> ifEmptyEnd
  where
    -- Workaround for passing empty passphrases as last capture in URL
    ifEmptyEnd = if last path == Just "" then [""] else []

data ApiError
    = HTTPStatusError Response
    | JSONDecodingError String
    | ServerError WalletError

instance showApiError :: Show ApiError where
    show (HTTPStatusError res) =
        "HTTPStatusError: " <> show (statusCode res) <> " msg: " <> statusMessage res
    show (JSONDecodingError e) =
        "JSONDecodingError: " <> show e
    show (ServerError e) =
        "ServerError: " <> gShow e

-- REQUESTS HELPERS

decodeResult :: forall a eff. Generic a => String -> Either Error a
decodeResult = either (Left <<< mkJSONError) (lmap mkServerError) <<< decodeJson <=< lmap error <<< jsonParser
  where
    mkJSONError = error <<< show <<< JSONDecodingError
    mkServerError = error <<< show <<< ServerError

makeRequest :: forall eff a. (Generic a) => (Request -> Eff (http :: HTTP, err :: EXCEPTION | eff) Unit) -> TLSOptions -> URLPath -> Aff (http :: HTTP, err :: EXCEPTION | eff) a
makeRequest withReq tls urlPath = do
    -- FIXME: exceptin shouldn't happen here?
    res <- makeAff $ const $ withReq <=< request (tls <> path := backendApi urlPath)
    when (isHttpError res) $
        throwError <<< error <<< show $ HTTPStatusError res
    rawData <- makeAff $ const $ onDataString (responseAsStream res) UTF8
    either throwError pure $ decodeResult rawData
  where
    isHttpError res = statusCode res >= 400

plainRequest :: forall eff a r. (Generic a) => TLSOptions -> URLPath -> Aff (http :: HTTP, err :: EXCEPTION | eff) a
plainRequest = makeRequest $ void <<< pure

bodyRequest :: forall eff a b. (Generic a, Generic b) => TLSOptions -> URLPath -> b -> Aff (http :: HTTP, err :: EXCEPTION | eff) a
bodyRequest tls urlPath body = makeRequest flushBody (tls <> headers := reqHeaders) urlPath
  where
    flushBody req = do
        let writeStream = requestAsStream req
        writeString writeStream UTF8 (show $ encodeJson body) $ pure unit
        end writeStream $ pure unit
    -- TODO: use Data.MediaType.Common here
    reqHeaders = RequestHeaders $ fromFoldable [Tuple "Content-Type" "application/json"]

getR :: forall eff a. Generic a => TLSOptions -> URLPath -> Aff (http :: HTTP, err :: EXCEPTION | eff) a
getR tls = plainRequest $ tls <> method := (show GET)

postR :: forall eff a. Generic a => TLSOptions -> URLPath -> Aff (http :: HTTP, err :: EXCEPTION | eff) a
postR tls = plainRequest $ tls <> method := (show POST)

postRBody :: forall eff a b. (Generic a, Generic b) => TLSOptions -> URLPath -> b -> Aff (http :: HTTP, err :: EXCEPTION | eff) a
postRBody tls urlPath = flip bodyRequest urlPath $ tls <> method  := (show POST)

putRBody :: forall eff a b. (Generic a, Generic b) => TLSOptions -> URLPath -> b -> Aff (http :: HTTP, err :: EXCEPTION | eff) a
putRBody tls urlPath = flip bodyRequest urlPath $ tls <> method  := (show PUT)

deleteR :: forall eff a. Generic a => TLSOptions -> URLPath -> Aff (http :: HTTP, err :: EXCEPTION | eff) a
deleteR tls = plainRequest $ tls <> method := (show DELETE)

-- REQUESTS
--------------------------------------------------------------------------------
-- TEST ------------------------------------------------------------------------
testReset :: forall eff. TLSOptions -> Aff (http :: HTTP, err :: EXCEPTION | eff) Unit
testReset tls = postR tls ["test", "reset"]
--------------------------------------------------------------------------------
-- WALLETS ---------------------------------------------------------------------
getWallets :: forall eff. TLSOptions -> Aff (http :: HTTP, err :: EXCEPTION | eff) (Array CWallet)
getWallets tls = getR tls ["wallets"]

getWallet :: forall eff. TLSOptions -> CAddress -> Aff (http :: HTTP, err :: EXCEPTION | eff) CWallet
getWallet tls addr = getR tls ["wallets", _address addr]

updateWallet :: forall eff. TLSOptions -> CAddress -> CWalletMeta -> Aff (http :: HTTP, err :: EXCEPTION | eff) CWallet
updateWallet tls addr = putRBody tls ["wallets", _address addr]

newWallet :: forall eff. TLSOptions -> CPassPhrase -> CWalletInit -> Aff (http :: HTTP, err :: EXCEPTION | eff) CWallet
newWallet tls pass = postRBody tls ["wallets", _passPhrase pass]

deleteWallet :: forall eff. TLSOptions -> CAddress -> Aff (http :: HTTP, err :: EXCEPTION | eff) Unit
deleteWallet tls addr = deleteR tls ["wallets", _address addr]

importKey :: forall eff. TLSOptions -> String -> Aff (http :: HTTP, err :: EXCEPTION | eff) CWallet
importKey tls = postRBody tls ["wallets", "keys"]

restoreWallet :: forall eff. TLSOptions -> CPassPhrase -> CWalletInit -> Aff (http :: HTTP, err :: EXCEPTION | eff) CWallet
restoreWallet tls pass = postRBody tls ["wallets", "restore", _passPhrase pass]
--------------------------------------------------------------------------------
-- ADDRESSSES ------------------------------------------------------------------
isValidAddress :: forall eff. TLSOptions -> CCurrency -> String -> Aff (http :: HTTP, err :: EXCEPTION | eff) Boolean
isValidAddress tls cCurrency addr = getR tls ["addresses", addr, "currencies", showCCurrency cCurrency]
--------------------------------------------------------------------------------
-- PROFILES --------------------------------------------------------------------
getProfile :: forall eff. TLSOptions -> Aff (http :: HTTP, err :: EXCEPTION | eff) CProfile
getProfile tls = getR tls ["profile"]

updateProfile :: forall eff. TLSOptions -> CProfile -> Aff (http :: HTTP, err :: EXCEPTION | eff) CProfile
updateProfile tls = postRBody tls ["profile"]
--------------------------------------------------------------------------------
-- TRANSACTIONS ----------------------------------------------------------------
send :: forall eff. TLSOptions -> CPassPhrase -> CAddress -> CAddress -> CCoin -> Aff (http :: HTTP, err :: EXCEPTION | eff) CTx
send tls pass addrFrom addrTo amount = postR tls ["txs", "payments", _passPhrase pass, _address addrFrom, _address addrTo, _ccoin amount]

sendExtended :: forall eff. TLSOptions -> CPassPhrase -> CAddress -> CAddress -> CCoin -> CCurrency -> String -> String -> Aff (http :: HTTP, err :: EXCEPTION | eff) CTx
sendExtended tls pass addrFrom addrTo amount curr title desc = postR tls ["txs", "payments", _passPhrase pass, _address addrFrom, _address addrTo, _ccoin amount, showCCurrency curr, title, desc]

updateTransaction :: forall eff. TLSOptions -> CAddress -> CTxId -> CTxMeta -> Aff (http :: HTTP, err :: EXCEPTION | eff) Unit
updateTransaction tls addr ctxId = postRBody tls ["txs", "payments", _address addr, _ctxIdValue ctxId]

getHistory :: forall eff. TLSOptions -> CAddress -> Int -> Int -> Aff (http :: HTTP, err :: EXCEPTION | eff) (Tuple (Array CTx) Int)
getHistory tls addr skip limit = getR tls ["txs", "histories", _address addr <> "?skip=" <> show skip <> "&limit=" <> show limit]

searchHistory :: forall eff. TLSOptions -> CAddress -> String -> Int -> Int -> Aff (http :: HTTP, err :: EXCEPTION | eff) (Tuple (Array CTx) Int)
searchHistory tls addr search skip limit = getR tls ["txs", "histories", _address addr, search <> "?skip=" <> show skip <> "&limit=" <> show limit]
--------------------------------------------------------------------------------
-- UPDATES ---------------------------------------------------------------------
nextUpdate :: forall eff. TLSOptions -> Aff (http :: HTTP, err :: EXCEPTION | eff) CUpdateInfo
nextUpdate tls = getR tls ["update"]

applyUpdate :: forall eff. TLSOptions -> Aff (http :: HTTP, err :: EXCEPTION | eff) Unit
applyUpdate tls = postR tls ["update"]
--------------------------------------------------------------------------------
-- REDEMPTIONS -----------------------------------------------------------------
redeemAda :: forall eff. TLSOptions -> CWalletRedeem -> Aff (http :: HTTP, err :: EXCEPTION | eff) CTx
redeemAda tls = postRBody tls ["redemptions", "ada"]

redeemAdaPaperVend :: forall eff. TLSOptions -> CPaperVendWalletRedeem -> Aff (http :: HTTP, err :: EXCEPTION | eff) CTx
redeemAdaPaperVend tls = postRBody tls ["papervend", "redemptions", "ada"]
--------------------------------------------------------------------------------
-- REPORTING ---------------------------------------------------------------------
reportInit :: forall eff. TLSOptions -> CInitialized -> Aff (http :: HTTP, err :: EXCEPTION | eff) Unit
reportInit tls = postRBody tls ["reporting", "initialized"]
--------------------------------------------------------------------------------
-- SETTINGS ---------------------------------------------------------------------
blockchainSlotDuration :: forall eff. TLSOptions -> Aff (http :: HTTP, err :: EXCEPTION | eff) Int
blockchainSlotDuration tls = getR tls ["settings", "slots", "duration"]

systemVersion :: forall eff. TLSOptions -> Aff (http :: HTTP, err :: EXCEPTION | eff) SoftwareVersion
systemVersion tls = getR tls ["settings", "version"]

syncProgress :: forall eff. TLSOptions -> Aff (http :: HTTP, err :: EXCEPTION | eff) SyncProgress
syncProgress tls = getR tls ["settings", "sync", "progress"]
--------------------------------------------------------------------------------
