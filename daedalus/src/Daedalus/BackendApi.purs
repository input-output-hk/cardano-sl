module Daedalus.BackendApi where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error, Error)
import Control.Monad.Error.Class (throwError)
import Daedalus.Constants (backendPrefix)
import Daedalus.Types (CId, _address, _ccoin, CAccount, CTx, CAccountMeta, CTxId, CTxMeta, _ctxIdValue, WalletError, CProfile, CAccountInit, CUpdateInfo, SoftwareVersion, CWalletRedeem, SyncProgress, CInitialized, CPassPhrase, _passPhrase, CCoin, CPaperVendWalletRedeem, Wal, CWallet, CWalletInit, walletAddressToUrl, CAccountId, CAddress, Addr)
import Data.Array (last, catMaybes)
import Data.Monoid (mempty)
import Data.Bifunctor (lmap)
import Data.Argonaut (Json)
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Bifunctor (bimap)
import Data.Either (either, Either(Left))
import Data.Generic (class Generic, gShow)
import Data.HTTP.Method (Method(POST, PUT, DELETE))
import Data.Maybe (Maybe(Just))
import Data.MediaType.Common (applicationJSON)
import Data.String (joinWith)
import Data.Tuple (Tuple (..))
import Network.HTTP.Affjax (AffjaxResponse, affjax, defaultRequest, AJAX, URL, AffjaxRequest)
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.RequestHeader (RequestHeader(ContentType))
import Network.HTTP.StatusCode (StatusCode(..))

-- HELPERS

type URLPath = Tuple (Array String) QueryParams
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
mkUrl (Tuple urlPath params) = joinWith "/" urlPath <> "?" <> queryParamToString  params
  where queryParamToString = joinWith "&" <<< map (\(Tuple l r) -> l <> "=" <> r) <<< catMaybes <<< map (\(Tuple name mParam) -> Tuple <$> pure name <*> mParam)

backendApi :: URLPath -> URL
backendApi = mkUrl <<< lmap ((<>) [backendPrefix, "api"])

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
-- Test ------------------------------------------------------------------------
testReset :: forall eff. Aff (ajax :: AJAX | eff) Unit
testReset = postR $ noQueryParam ["test", "reset"]
--------------------------------------------------------------------------------
-- Wallets ---------------------------------------------------------------------
getWallet :: forall eff. CId Wal -> Aff (ajax :: AJAX | eff) CWallet
getWallet addr = getR $ noQueryParam ["wallets", _address addr]

getWallets :: forall eff. Aff (ajax :: AJAX | eff) (Array CWallet)
getWallets = getR $ noQueryParam ["wallets"]

newWallet :: forall eff. Maybe CPassPhrase -> CWalletInit -> Aff (ajax :: AJAX | eff) CWallet
newWallet pass = postRBody $ queryParams ["wallets", "new"] [qParam "passphrase" $ _passPhrase <$> pass]

restoreWallet :: forall eff. Maybe CPassPhrase -> CWalletInit -> Aff (ajax :: AJAX | eff) CWallet
restoreWallet pass = postRBody $ queryParams ["wallets", "restore"] [qParam "passphrase" $ _passPhrase <$> pass]

renameWalletSet :: forall eff. CId Wal -> String -> Aff (ajax :: AJAX | eff) CWallet
renameWalletSet wSetId name = postR $ noQueryParam ["wallets", "rename", _address wSetId, name]

importWallet :: forall eff. Maybe CPassPhrase -> FilePath -> Aff (ajax :: AJAX | eff) CWallet
importWallet pass = postRBody $ queryParams ["wallets", "keys"] [qParam "passphrase" $ _passPhrase <$> pass]

changeWalletPass :: forall eff. CId Wal -> Maybe CPassPhrase -> Maybe CPassPhrase -> Aff (ajax :: AJAX | eff) Unit
changeWalletPass wSetId old new = postR $ queryParams ["wallets", "password", _address wSetId] [qParam "old" $ _passPhrase <$> old, qParam "new" $ _passPhrase <$> new]

deleteWallet :: forall eff. CId Wal -> Aff (ajax :: AJAX | eff) Unit
deleteWallet wSetId = deleteR $ noQueryParam ["wallets", _address wSetId]
--------------------------------------------------------------------------------
-- Accounts --------------------------------------------------------------------

getAccount :: forall eff. CAccountId -> Aff (ajax :: AJAX | eff) CAccount
getAccount wId = getR $ noQueryParam ["accounts", walletAddressToUrl wId]

getAccounts :: forall eff. Maybe (CId Wal) -> Aff (ajax :: AJAX | eff) (Array CAccount)
getAccounts addr = getR $ queryParams ["accounts"] [qParam "accountId" $ _address <$> addr]

updateAccount :: forall eff. CAccountId -> CAccountMeta -> Aff (ajax :: AJAX | eff) CAccount
updateAccount wId = putRBody $ noQueryParam ["accounts", walletAddressToUrl wId]

newAccount :: forall eff. Maybe CPassPhrase -> CAccountInit -> Aff (ajax :: AJAX | eff) CAccount
newAccount pass = postRBody $ queryParams ["accounts"] [qParam "passphrase" $ _passPhrase <$> pass]

deleteAccount :: forall eff. CAccountId -> Aff (ajax :: AJAX | eff) Unit
deleteAccount wId = deleteR $ noQueryParam ["accounts", walletAddressToUrl wId]

--------------------------------------------------------------------------------
-- Wallet addresses ------------------------------------------------------------

newWAddress :: forall eff. Maybe CPassPhrase -> CAccountId -> Aff (ajax :: AJAX | eff) CAddress
newWAddress pass = postRBody $ queryParams ["addresses"] [qParam "passphrase" $ _passPhrase <$> pass]

--------------------------------------------------------------------------------
-- Addresses -------------------------------------------------------------------

isValidAddress :: forall eff. String -> Aff (ajax :: AJAX | eff) Boolean
isValidAddress addr = getR $ noQueryParam ["addresses", addr]

--------------------------------------------------------------------------------
-- Profiles --------------------------------------------------------------------
getProfile :: forall eff. Aff (ajax :: AJAX | eff) CProfile
getProfile = getR $ noQueryParam ["profile"]

updateProfile :: forall eff. CProfile -> Aff (ajax :: AJAX | eff) CProfile
updateProfile = postRBody $ noQueryParam ["profile"]
--------------------------------------------------------------------------------
-- Transactions ----------------------------------------------------------------
newPayment :: forall eff. Maybe CPassPhrase -> CAccountId -> CId Addr -> CCoin -> Aff (ajax :: AJAX | eff) CTx
newPayment pass addrFrom addrTo amount = postR $ queryParams ["txs", "payments", walletAddressToUrl addrFrom, _address addrTo, _ccoin amount] [qParam "passphrase" $ _passPhrase <$> pass]

newPaymentExtended :: forall eff. Maybe CPassPhrase -> CAccountId -> CId Addr -> CCoin -> String -> String -> Aff (ajax :: AJAX | eff) CTx
newPaymentExtended pass addrFrom addrTo amount title desc = postR $ queryParams ["txs", "payments", walletAddressToUrl  addrFrom, _address addrTo, _ccoin amount, title, desc] [qParam "passphrase" $ _passPhrase <$> pass]

updateTransaction :: forall eff. CAccountId -> CTxId -> CTxMeta -> Aff (ajax :: AJAX | eff) Unit
updateTransaction addr ctxId = postRBody $ noQueryParam ["txs", "payments", walletAddressToUrl addr, _ctxIdValue ctxId]

getHistory :: forall eff. CAccountId -> Maybe Int -> Maybe Int -> Aff (ajax :: AJAX | eff) (Tuple (Array CTx) Int)
getHistory addr skip limit = getR $ queryParams ["txs", "histories", walletAddressToUrl addr] [qParam "skip" $ show <$> skip, qParam "limit" $ show <$> limit]

searchHistory :: forall eff. CAccountId -> Maybe (CId Addr) -> String -> Maybe Int -> Maybe Int -> Aff (ajax :: AJAX | eff) (Tuple (Array CTx) Int)
searchHistory addr account search skip limit = getR $ queryParams ["txs", "histories", walletAddressToUrl addr, search] [qParam "account" $ _address <$> account, qParam "skip" $ show <$> skip, qParam "limit" $ show <$> limit]
--------------------------------------------------------------------------------
-- Updates ---------------------------------------------------------------------
nextUpdate :: forall eff. Aff (ajax :: AJAX | eff) CUpdateInfo
nextUpdate = getR $ noQueryParam ["update"]

applyUpdate :: forall eff. Aff (ajax :: AJAX | eff) Unit
applyUpdate = postR $ noQueryParam ["update"]
--------------------------------------------------------------------------------
-- Redemptions -----------------------------------------------------------------
redeemAda :: forall eff. Maybe CPassPhrase -> CWalletRedeem -> Aff (ajax :: AJAX | eff) CTx
redeemAda pass = postRBody $ queryParams ["redemptions", "ada"] [qParam "passphrase" $ _passPhrase <$> pass]

redeemAdaPaperVend :: forall eff. Maybe CPassPhrase -> CPaperVendWalletRedeem -> Aff (ajax :: AJAX | eff) CTx
redeemAdaPaperVend pass = postRBody $ queryParams ["papervend", "redemptions", "ada"] [qParam "passphrase" $ _passPhrase <$> pass]
--------------------------------------------------------------------------------
-- REPORTING ---------------------------------------------------------------------
reportInit :: forall eff. CInitialized -> Aff (ajax :: AJAX | eff) Unit
reportInit = postRBody $ noQueryParam ["reporting", "initialized"]

--------------------------------------------------------------------------------
-- SETTINGS ---------------------------------------------------------------------
blockchainSlotDuration :: forall eff. Aff (ajax :: AJAX | eff) Int
blockchainSlotDuration = getR $ noQueryParam ["settings", "slots", "duration"]

systemVersion :: forall eff. Aff (ajax :: AJAX | eff) SoftwareVersion
systemVersion = getR $ noQueryParam ["settings", "version"]

syncProgress :: forall eff. Aff (ajax :: AJAX | eff) SyncProgress
syncProgress = getR $ noQueryParam ["settings", "sync", "progress"]
--------------------------------------------------------------------------------
