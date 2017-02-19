module Daedalus.BackendApi where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error, Error)
import Control.Monad.Error.Class (throwError)
import Daedalus.Constants (backendPrefix)
import Daedalus.Types (CAddress, Coin, _address, _coin, CWallet, CTx, CWalletMeta, CTxId, CTxMeta, _ctxIdValue, CCurrency, WalletError, showCCurrency, CProfile, CWalletInit, BackupPhrase, CUpdateInfo, SoftwareVersion, CWalletRedeem)
import Data.Argonaut (Json)
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Bifunctor (bimap)
import Data.Either (either, Either(Left))
import Data.Generic (class Generic, gShow)
import Data.HTTP.Method (Method(POST))
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
backendApi path = mkUrl $ [backendPrefix, "api"] <> path

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

-- REQUESTS
getProfile :: forall eff. Aff (ajax :: AJAX | eff) CProfile
getProfile = getR ["get_profile"]

updateProfile :: forall eff. CProfile -> Aff (ajax :: AJAX | eff) CProfile
updateProfile = postRBody ["update_profile"]

getWallets :: forall eff. Aff (ajax :: AJAX | eff) (Array CWallet)
getWallets = getR ["get_wallets"]

getWallet :: forall eff. CAddress -> Aff (ajax :: AJAX | eff) CWallet
getWallet addr = getR ["get_wallet", _address addr]

getHistory :: forall eff. CAddress -> Int -> Int -> Aff (ajax :: AJAX | eff) (Tuple (Array CTx) Int)
getHistory addr skip limit = getR ["txhistory", _address addr, show skip, show limit]

searchHistory :: forall eff. CAddress -> String -> Int -> Int -> Aff (ajax :: AJAX | eff) (Tuple (Array CTx) Int)
searchHistory addr search skip limit = getR ["search_txhistory", _address addr, search, show skip, show limit]

send :: forall eff. CAddress -> CAddress -> Coin -> Aff (ajax :: AJAX | eff) CTx
send addrFrom addrTo amount = postR ["send", _address addrFrom, _address addrTo, show $ _coin amount]

sendExtended :: forall eff. CAddress -> CAddress -> Coin -> CCurrency -> String -> String -> Aff (ajax :: AJAX | eff) CTx
sendExtended addrFrom addrTo amount curr title desc = postR ["send", _address addrFrom, _address addrTo, show $ _coin amount, showCCurrency curr, title, desc]

newWallet :: forall eff. CWalletInit -> Aff (ajax :: AJAX | eff) CWallet
newWallet = postRBody ["new_wallet"]

updateTransaction :: forall eff. CAddress -> CTxId -> CTxMeta -> Aff (ajax :: AJAX | eff) Unit
updateTransaction addr ctxId = postRBody ["update_transaction", _address addr, _ctxIdValue ctxId]

updateWallet :: forall eff. CAddress -> CWalletMeta -> Aff (ajax :: AJAX | eff) CWallet
updateWallet addr = postRBody ["update_wallet", _address addr]

-- FIXME: use DELETE method
deleteWallet :: forall eff. CAddress -> Aff (ajax :: AJAX | eff) Unit
deleteWallet addr = postR ["delete_wallet", _address addr]

isValidAddress :: forall eff. CCurrency -> String -> Aff (ajax :: AJAX | eff) Boolean
isValidAddress cCurrency addr = getR ["valid_address", showCCurrency cCurrency, addr]

blockchainSlotDuration :: forall eff. Aff (ajax :: AJAX | eff) Int
blockchainSlotDuration = getR ["slot_duration"]

restoreWallet :: forall eff. CWalletInit -> Aff (ajax :: AJAX | eff) CWallet
restoreWallet = postRBody ["restore_wallet"]

nextUpdate :: forall eff. Aff (ajax :: AJAX | eff) CUpdateInfo
nextUpdate = getR ["next_update"]

applyUpdate :: forall eff. Aff (ajax :: AJAX | eff) Unit
applyUpdate = postR ["apply_update"]

systemVersion :: forall eff. Aff (ajax :: AJAX | eff) SoftwareVersion
systemVersion = getR ["system_version"]

redeemADA :: forall eff. CWalletRedeem -> Aff (ajax :: AJAX | eff) CWallet
redeemADA = postRBody ["redeem_ada"]

importKey :: forall eff. String -> Aff (ajax :: AJAX | eff) CWallet
importKey = postRBody ["import_key"]
