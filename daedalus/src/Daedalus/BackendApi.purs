module Daedalus.BackendApi where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Exception (error, Error, EXCEPTION)
import Control.Monad.Eff.Ref.Unsafe (unsafeRunRef)
import Control.Monad.Eff.Ref (modifyRef, newRef, readRef)
import Control.Monad.Error.Class (throwError)
import Daedalus.Types (CId, _address, _ccoin, CAccount, CTx, CAccountMeta, CTxId, CTxMeta, _ctxIdValue, WalletError, CProfile, CAccountInit, CUpdateInfo, SoftwareVersion, CWalletRedeem, SyncProgress, CInitialized, CPassPhrase, _passPhrase, CCoin, CPaperVendWalletRedeem, Wal, CWallet, CWalletInit, walletAddressToUrl, CAccountId, CAddress, Addr, CWalletMeta, CFilePath (..), InputSelectionPolicy)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (either, Either(Left))
import Data.FormURLEncoded (fromArray, encode)
import Data.Generic (class Generic, gShow)
import Data.HTTP.Method (Method(GET, POST, PUT, DELETE))
import Data.Maybe (Maybe)
import Data.Monoid (mempty)
import Data.String (joinWith)
import Data.List.Lazy (cons, nil, reverse, toUnfoldable)
import Data.Tuple (Tuple (..))
import Data.StrMap (fromFoldable)
import Daedalus.TLS (TLSOptions)
import Node.HTTP.Client (method, path, request, statusCode, statusMessage, Response, responseAsStream, headers, RequestHeaders (..), Request, requestAsStream)
import Data.Options ((:=))
import Node.Encoding (Encoding (UTF8))
import Node.Stream (onDataString, onEnd, onError, writeString, end)
import Node.HTTP (HTTP)

-- HELPERS

type URLPath = Tuple (Array String) QueryParams
type URL = String
type QueryParam = Tuple String (Maybe String)
type QueryParams = Array QueryParam
type FilePath = String

noQueryParam :: Array String -> URLPath
noQueryParam = flip Tuple mempty

queryParams :: Array String -> QueryParams -> URLPath
queryParams = Tuple

qParam :: String -> Maybe String -> QueryParam
qParam = Tuple

mkUrl :: URLPath -> URL
mkUrl (Tuple urlPath params) = joinWith "/" urlPath <> "?" <> encode (fromArray params)

backendApi :: URLPath -> URL
backendApi = mkUrl <<< lmap ((<>) ["/api"])

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

decodeResult :: forall a. Generic a => String -> Either Error a
decodeResult = either (Left <<< mkJSONError) (lmap mkServerError) <<< decodeJson <=< lmap error <<< jsonParser
  where
    mkJSONError = error <<< show <<< JSONDecodingError
    mkServerError = error <<< show <<< ServerError

responseToString :: forall eff. Response -> Aff (http :: HTTP, exception :: EXCEPTION | eff) String
responseToString res = makeAff $ \withError withSuccess -> do
    let stream = responseAsStream res
    onError stream withError
    buf <- unsafeRunRef $ newRef nil
    onDataString stream UTF8 $ \strChunk -> do
        void $ unsafeRunRef $ modifyRef buf (cons strChunk)
    onEnd stream $ do
        strChunksReversed <- unsafeRunRef $ readRef buf
        let s = (joinWith "" <<< toUnfoldable <<< reverse) strChunksReversed
        withSuccess s

makeRequest :: forall eff a. Generic a => (Request -> Eff (http :: HTTP, exception :: EXCEPTION | eff) Unit)
    -> TLSOptions -> URLPath -> Aff (http :: HTTP, exception :: EXCEPTION | eff) a
makeRequest withReq tls urlPath = do
    -- FIXME: exceptin shouldn't happen here?
    res <- makeAff $ const $ withReq <=< request (tls <> path := backendApi urlPath)
    when (isHttpError res) $
        throwError <<< error <<< show $ HTTPStatusError res
    rawData <- responseToString res
    either throwError pure $ decodeResult rawData
  where
    isHttpError res = statusCode res >= 400

plainRequest :: forall eff a. Generic a => TLSOptions -> URLPath -> Aff (http :: HTTP, exception :: EXCEPTION | eff) a
plainRequest = makeRequest $ flip end (pure unit) <<< requestAsStream

bodyRequest :: forall eff a b. Generic a => Generic b => TLSOptions -> URLPath -> b -> Aff (http :: HTTP, exception :: EXCEPTION | eff) a
bodyRequest tls urlPath body = makeRequest flushBody (tls <> headers := reqHeaders) urlPath
  where
    flushBody req = do
        let writeStream = requestAsStream req
        _ <- writeString writeStream UTF8 (show $ encodeJson body) $ pure unit
        end writeStream $ pure unit
    -- TODO: use Data.MediaType.Common here
    reqHeaders = RequestHeaders $ fromFoldable [Tuple "Content-Type" "application/json"]

getR :: forall eff a. Generic a => TLSOptions -> URLPath -> Aff (http :: HTTP, exception :: EXCEPTION | eff) a
getR tls = plainRequest $ tls <> method := (show GET)

postR :: forall eff a. Generic a => TLSOptions -> URLPath -> Aff (http :: HTTP, exception :: EXCEPTION | eff) a
postR tls = plainRequest $ tls <> method := (show POST)

postRBody :: forall eff a b. Generic a => Generic b => TLSOptions -> URLPath -> b -> Aff (http :: HTTP, exception :: EXCEPTION | eff) a
postRBody tls urlPath = flip bodyRequest urlPath $ tls <> method  := (show POST)

putRBody :: forall eff a b. Generic a => Generic b => TLSOptions -> URLPath -> b -> Aff (http :: HTTP, exception :: EXCEPTION | eff) a
putRBody tls urlPath = flip bodyRequest urlPath $ tls <> method  := (show PUT)

deleteR :: forall eff a. Generic a => TLSOptions -> URLPath -> Aff (http :: HTTP, exception :: EXCEPTION | eff) a
deleteR tls = plainRequest $ tls <> method := (show DELETE)

-- REQUESTS
--------------------------------------------------------------------------------
-- Test ------------------------------------------------------------------------
testReset :: forall eff. TLSOptions -> Aff (http :: HTTP, exception :: EXCEPTION | eff) Unit
testReset tls = postR tls $ noQueryParam ["test", "reset"]
--------------------------------------------------------------------------------
-- Wallets ---------------------------------------------------------------------
getWallet :: forall eff. TLSOptions -> CId Wal -> Aff (http :: HTTP, exception :: EXCEPTION | eff) CWallet
getWallet tls addr = getR tls $ noQueryParam ["wallets", _address addr]

getWallets :: forall eff. TLSOptions -> Aff (http :: HTTP, exception :: EXCEPTION | eff) (Array CWallet)
getWallets tls = getR tls $ noQueryParam ["wallets"]

newWallet :: forall eff. TLSOptions -> Maybe CPassPhrase -> CWalletInit -> Aff (http :: HTTP, exception :: EXCEPTION | eff) CWallet
newWallet tls pass = postRBody tls $ queryParams ["wallets", "new"] [qParam "passphrase" $ _passPhrase <$> pass]

updateWallet :: forall eff. TLSOptions -> CId Wal -> CWalletMeta -> Aff (http :: HTTP, exception :: EXCEPTION | eff) CWallet
updateWallet tls wId = putRBody tls $ noQueryParam ["wallets", _address wId]

restoreWallet :: forall eff. TLSOptions -> Maybe CPassPhrase -> CWalletInit -> Aff (http :: HTTP, exception :: EXCEPTION | eff) CWallet
restoreWallet tls pass = postRBody tls $ queryParams ["wallets", "restore"] [qParam "passphrase" $ _passPhrase <$> pass]

renameWalletSet :: forall eff. TLSOptions -> CId Wal -> String -> Aff (http :: HTTP, exception :: EXCEPTION | eff) CWallet
renameWalletSet tls wSetId name = postR tls $ noQueryParam ["wallets", "rename", _address wSetId, name]

importWallet :: forall eff. TLSOptions -> Maybe CPassPhrase -> CFilePath -> Aff (http :: HTTP, exception :: EXCEPTION | eff) CWallet
importWallet tls pass (CFilePath path) = postRBody tls (queryParams ["wallets", "keys"] [qParam "passphrase" $ _passPhrase <$> pass]) path

changeWalletPass :: forall eff. TLSOptions -> CId Wal -> Maybe CPassPhrase -> Maybe CPassPhrase -> Aff (http :: HTTP, exception :: EXCEPTION | eff) Unit
changeWalletPass tls wSetId old new = postR tls $ queryParams ["wallets", "password", _address wSetId] [qParam "old" $ _passPhrase <$> old, qParam "new" $ _passPhrase <$> new]

deleteWallet :: forall eff. TLSOptions -> CId Wal -> Aff (http :: HTTP, exception :: EXCEPTION | eff) Unit
deleteWallet tls wSetId = deleteR tls $ noQueryParam ["wallets", _address wSetId]
--------------------------------------------------------------------------------
-- Accounts --------------------------------------------------------------------

getAccount :: forall eff. TLSOptions -> CAccountId -> Aff (http :: HTTP, exception :: EXCEPTION | eff) CAccount
getAccount tls wId = getR tls $ noQueryParam ["accounts", walletAddressToUrl wId]

getAccounts :: forall eff. TLSOptions -> Maybe (CId Wal) -> Aff (http :: HTTP, exception :: EXCEPTION | eff) (Array CAccount)
getAccounts tls addr = getR tls $ queryParams ["accounts"] [qParam "accountId" $ _address <$> addr]

updateAccount :: forall eff. TLSOptions -> CAccountId -> CAccountMeta -> Aff (http :: HTTP, exception :: EXCEPTION | eff) CAccount
updateAccount tls wId = putRBody tls $ noQueryParam ["accounts", walletAddressToUrl wId]

newAccount :: forall eff. TLSOptions -> Maybe CPassPhrase -> CAccountInit -> Aff (http :: HTTP, exception :: EXCEPTION | eff) CAccount
newAccount tls pass = postRBody tls $ queryParams ["accounts"] [qParam "passphrase" $ _passPhrase <$> pass]

deleteAccount :: forall eff. TLSOptions -> CAccountId -> Aff (http :: HTTP, exception :: EXCEPTION | eff) Unit
deleteAccount tls wId = deleteR tls $ noQueryParam ["accounts", walletAddressToUrl wId]

--------------------------------------------------------------------------------
-- Wallet addresses ------------------------------------------------------------

newAddress :: forall eff. TLSOptions -> Maybe CPassPhrase -> CAccountId -> Aff (http :: HTTP, exception :: EXCEPTION | eff) CAddress
newAddress tls pass = postRBody tls $ queryParams ["addresses"] [qParam "passphrase" $ _passPhrase <$> pass]

--------------------------------------------------------------------------------
-- Addresses -------------------------------------------------------------------

isValidAddress :: forall eff. TLSOptions -> (CId Addr) -> Aff (http :: HTTP, exception :: EXCEPTION | eff) Boolean
isValidAddress tls addr = getR tls $ noQueryParam ["addresses", _address addr]

--------------------------------------------------------------------------------
-- Profiles --------------------------------------------------------------------
getProfile :: forall eff. TLSOptions -> Aff (http :: HTTP, exception :: EXCEPTION | eff) CProfile
getProfile tls = getR tls $ noQueryParam ["profile"]

updateProfile :: forall eff. TLSOptions -> CProfile -> Aff (http :: HTTP, exception :: EXCEPTION | eff) CProfile
updateProfile tls = postRBody tls $ noQueryParam ["profile"]
--------------------------------------------------------------------------------
-- Transactions ----------------------------------------------------------------
newPayment :: forall eff. TLSOptions -> Maybe CPassPhrase -> CAccountId -> CId Addr -> CCoin -> Maybe InputSelectionPolicy -> Aff (http :: HTTP, exception :: EXCEPTION | eff) CTx
newPayment tls pass addrFrom addrTo amount = postRBody tls $ queryParams ["txs", "payments", walletAddressToUrl addrFrom, _address addrTo, _ccoin amount] [qParam "passphrase" $ _passPhrase <$> pass]

txFee :: forall eff. TLSOptions -> CAccountId -> CId Addr -> CCoin -> Maybe InputSelectionPolicy -> Aff (http :: HTTP, exception :: EXCEPTION | eff) CCoin
txFee tls addrFrom addrTo amount = postRBody tls $ noQueryParam ["txs", "fee", walletAddressToUrl addrFrom, _address addrTo, _ccoin amount]

updateTransaction :: forall eff. TLSOptions -> CAccountId -> CTxId -> CTxMeta -> Aff (http :: HTTP, exception :: EXCEPTION | eff) Unit
updateTransaction tls addr ctxId = postRBody tls $ noQueryParam ["txs", "payments", walletAddressToUrl addr, _ctxIdValue ctxId]

getHistory
  :: forall eff.
     TLSOptions
  -> Maybe (CId Wal)
  -> Maybe CAccountId
  -> Maybe (CId Addr)
  -> Maybe Int
  -> Maybe Int
  -> Aff (http :: HTTP, exception :: EXCEPTION | eff) (Tuple (Array CTx) Int)
getHistory tls walletId accountId addr skip limit =
  getR tls $ queryParams
  ["txs", "histories"]
  [ qParam "walletId" $ _address <$> walletId
  , qParam "accountId" $ walletAddressToUrl <$> accountId
  , qParam "address" $ _address <$> addr
  , qParam "skip" $ show <$> skip
  , qParam "limit" $ show <$> limit
  ]

--------------------------------------------------------------------------------
-- Updates ---------------------------------------------------------------------
nextUpdate :: forall eff. TLSOptions -> Aff (http :: HTTP, exception :: EXCEPTION | eff) CUpdateInfo
nextUpdate tls = getR tls $ noQueryParam ["update"]

postponeUpdate :: forall eff. TLSOptions -> Aff (http :: HTTP, exception :: EXCEPTION | eff) Unit
postponeUpdate tls = postR tls $ noQueryParam ["update", "postpone"]

applyUpdate :: forall eff. TLSOptions -> Aff (http :: HTTP, exception :: EXCEPTION | eff) Unit
applyUpdate tls = postR tls $ noQueryParam ["update", "apply"]

--------------------------------------------------------------------------------
-- Redemptions -----------------------------------------------------------------
redeemAda :: forall eff. TLSOptions -> Maybe CPassPhrase -> CWalletRedeem -> Aff (http :: HTTP, exception :: EXCEPTION | eff) CTx
redeemAda tls pass = postRBody tls $ queryParams ["redemptions", "ada"] [qParam "passphrase" $ _passPhrase <$> pass]

redeemAdaPaperVend :: forall eff. TLSOptions -> Maybe CPassPhrase -> CPaperVendWalletRedeem -> Aff (http :: HTTP, exception :: EXCEPTION | eff) CTx
redeemAdaPaperVend tls pass = postRBody tls $ queryParams ["papervend", "redemptions", "ada"] [qParam "passphrase" $ _passPhrase <$> pass]

--------------------------------------------------------------------------------
-- REPORTING -------------------------------------------------------------------
reportInit :: forall eff. TLSOptions -> CInitialized -> Aff (http :: HTTP, exception :: EXCEPTION | eff) Unit
reportInit tls = postRBody tls $ noQueryParam ["reporting", "initialized"]

--------------------------------------------------------------------------------
-- SETTINGS --------------------------------------------------------------------
blockchainSlotDuration :: forall eff. TLSOptions -> Aff (http :: HTTP, exception :: EXCEPTION | eff) Int
blockchainSlotDuration tls = getR tls $ noQueryParam ["settings", "slots", "duration"]

systemVersion :: forall eff. TLSOptions -> Aff (http :: HTTP, exception :: EXCEPTION | eff) SoftwareVersion
systemVersion tls = getR tls $ noQueryParam ["settings", "version"]

syncProgress :: forall eff. TLSOptions -> Aff (http :: HTTP, exception :: EXCEPTION | eff) SyncProgress
syncProgress tls = getR tls $ noQueryParam ["settings", "sync", "progress"]

-- Endpoint to check local time differences
-- The time unit of the return value is Microsecond (see https://en.wikipedia.org/wiki/Microsecond)
-- * `0` == no difference
-- * All of `> 0` == difference in Microsecond
localTimeDifference :: forall eff. TLSOptions -> Aff (http :: HTTP, exception :: EXCEPTION | eff) Int
localTimeDifference tls = getR tls $ noQueryParam ["settings", "time", "difference"]

--------------------------------------------------------------------------------
-- JSON BACKUP -----------------------------------------------------------------
importBackupJSON :: forall eff. TLSOptions -> CFilePath -> Aff (http :: HTTP, exception :: EXCEPTION | eff) CWallet
importBackupJSON tls (CFilePath path) = postRBody tls (noQueryParam ["backup", "import"]) path

exportBackupJSON :: forall eff. TLSOptions -> CId Wal -> CFilePath -> Aff (http :: HTTP, exception :: EXCEPTION | eff) Unit
exportBackupJSON tls addr (CFilePath path) = postRBody tls (noQueryParam ["backup", "export", _address addr]) path
