module Daedalus.BackendApi where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error, Error)
import Control.Monad.Error.Class (throwError)
import Daedalus.Constants (backendPrefix)
import Daedalus.Types (CAddress, _address, _ccoin, CWallet, CTx, CWalletMeta, CTxId, CTxMeta, _ctxIdValue, WalletError, CProfile, CWalletInit, CUpdateInfo, SoftwareVersion, CWalletRedeem, SyncProgress, CInitialized, CPassPhrase, _passPhrase, CCoin, CPaperVendWalletRedeem, WS, CWalletSet, CWalletSetInit, walletAddressToUrl, CWalletAddress, CAccount, Acc)
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
-- Wallet Sets ---------------------------------------------------------------------
getWalletSet :: forall eff. CAddress WS -> Aff (ajax :: AJAX | eff) CWalletSet
getWalletSet addr = getR $ noQueryParam ["wallets", "sets", _address addr]

getWalletSets :: forall eff. Aff (ajax :: AJAX | eff) (Array CWalletSet)
getWalletSets = getR $ noQueryParam ["wallets", "sets"]

newWalletSet :: forall eff. Maybe CPassPhrase -> CWalletSetInit -> Aff (ajax :: AJAX | eff) CWalletSet
newWalletSet pass = postRBody $ queryParams ["wallets", "sets", "new"] [qParam "passphrase" $ _passPhrase <$> pass]

restoreWalletSet :: forall eff. Maybe CPassPhrase -> CWalletSetInit -> Aff (ajax :: AJAX | eff) CWalletSet
restoreWalletSet pass = postRBody $ queryParams ["wallets", "sets", "restore"] [qParam "passphrase" $ _passPhrase <$> pass]

renameWalletSet :: forall eff. CAddress WS -> String -> Aff (ajax :: AJAX | eff) CWalletSet
renameWalletSet wSetId name = postR $ noQueryParam ["wallets", "sets", "rename", _address wSetId, name]

importWalletSet :: forall eff. Maybe CPassPhrase -> FilePath -> Aff (ajax :: AJAX | eff) CWalletSet
importWalletSet pass = postRBody $ queryParams ["wallets", "sets", "keys"] [qParam "passphrase" $ _passPhrase <$> pass]

changeWalletSetPass :: forall eff. CAddress WS -> Maybe CPassPhrase -> Maybe CPassPhrase -> Aff (ajax :: AJAX | eff) Unit
changeWalletSetPass wSetId old new = postR $ queryParams ["wallets", "sets", "password", _address wSetId] [qParam "old" $ _passPhrase <$> old, qParam "new" $ _passPhrase <$> new]

deleteWalletSet :: forall eff. CAddress WS -> Aff (ajax :: AJAX | eff) Unit
deleteWalletSet wSetId = deleteR $ noQueryParam ["wallets", "sets", _address wSetId]
--------------------------------------------------------------------------------
-- Wallets ---------------------------------------------------------------------

getWallet :: forall eff. CWalletAddress -> Aff (ajax :: AJAX | eff) CWallet
getWallet wId = getR $ noQueryParam ["wallets", walletAddressToUrl wId]

getWallets :: forall eff. Maybe (CAddress WS) -> Aff (ajax :: AJAX | eff) (Array CWallet)
getWallets addr = getR $ queryParams ["wallets"] [qParam "walletSetId" $ _address <$> addr]

updateWallet :: forall eff. CWalletAddress -> CWalletMeta -> Aff (ajax :: AJAX | eff) CWallet
updateWallet wId = putRBody $ noQueryParam ["wallets", walletAddressToUrl wId]

newWallet :: forall eff. Maybe CPassPhrase -> CWalletInit -> Aff (ajax :: AJAX | eff) CWallet
newWallet pass = postRBody $ queryParams ["wallets"] [qParam "passphrase" $ _passPhrase <$> pass]

deleteWallet :: forall eff. CWalletAddress -> Aff (ajax :: AJAX | eff) Unit
deleteWallet wId = deleteR $ noQueryParam ["wallets", walletAddressToUrl wId]

--------------------------------------------------------------------------------
-- Accounts --------------------------------------------------------------------

newAccount :: forall eff. Maybe CPassPhrase -> CWalletAddress -> Aff (ajax :: AJAX | eff) CAccount
newAccount pass = postRBody $ queryParams ["account"] [qParam "passphrase" $ _passPhrase <$> pass]

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
newPayment :: forall eff. Maybe CPassPhrase -> CWalletAddress -> CAddress Acc -> CCoin -> Aff (ajax :: AJAX | eff) CTx
newPayment pass addrFrom addrTo amount = postR $ queryParams ["txs", "payments", walletAddressToUrl addrFrom, _address addrTo, _ccoin amount] [qParam "passphrase" $ _passPhrase <$> pass]

newPaymentExtended :: forall eff. Maybe CPassPhrase -> CWalletAddress -> CAddress Acc -> CCoin -> String -> String -> Aff (ajax :: AJAX | eff) CTx
newPaymentExtended pass addrFrom addrTo amount title desc = postR $ queryParams ["txs", "payments", walletAddressToUrl  addrFrom, _address addrTo, _ccoin amount, title, desc] [qParam "passphrase" $ _passPhrase <$> pass]

updateTransaction :: forall eff. CWalletAddress -> CTxId -> CTxMeta -> Aff (ajax :: AJAX | eff) Unit
updateTransaction addr ctxId = postRBody $ noQueryParam ["txs", "payments", walletAddressToUrl addr, _ctxIdValue ctxId]

getHistory :: forall eff. CWalletAddress -> Maybe Int -> Maybe Int -> Aff (ajax :: AJAX | eff) (Tuple (Array CTx) Int)
getHistory addr skip limit = getR $ queryParams ["txs", "histories", walletAddressToUrl addr] [qParam "skip" $ show <$> skip, qParam "limit" $ show <$> limit]

searchHistory :: forall eff. CWalletAddress -> Maybe (CAddress Acc) -> String -> Maybe Int -> Maybe Int -> Aff (ajax :: AJAX | eff) (Tuple (Array CTx) Int)
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
