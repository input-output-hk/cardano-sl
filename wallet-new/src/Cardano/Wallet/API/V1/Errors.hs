module Cardano.Wallet.API.V1.Errors where

import           Universum

import           Cardano.Wallet.API.V1.Headers (applicationJson)
import           Data.Aeson (ToJSON, encode)
import           Formatting (build, sformat)
import           Servant (ServantErr (..))

import qualified Network.HTTP.Types as HTTP


class (ToJSON e) => ToServantError e where
    declareServantError :: e -> ServantErr
    toServantError :: e -> ServantErr
    toServantError err =
        mkServantErr (declareServantError err)
      where
        mkServantErr serr@ServantErr{..} = serr
            { errBody    = encode err
            , errHeaders = applicationJson : errHeaders
            }

class (ToServantError e, Buildable e) => ToHttpErrorStatus e where
    toHttpErrorStatus :: e -> HTTP.Status
    toHttpErrorStatus err =
        HTTP.Status (errHTTPCode $ toServantError err) (encodeUtf8 $ sformat build err)
