{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

module Cardano.Node.Client
    ( -- Node Client
      NodeClient(..)
    , ClientError(..)
    , fromServantError

    -- * HTTP instance
    , NodeHttpClient
    , mkHttpClient
    ) where

import           Universum

import           Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import           Network.HTTP.Client (Manager)
import           Network.HTTP.Media.MediaType (MediaType)
import           Servant ((:<|>) (..))
import           Servant.Client (BaseUrl (..), ClientEnv (..), ClientM,
                     GenResponse (..), Response, ServantError, client,
                     runClientM)
import qualified Servant.Client as Servant

import           Cardano.Node.API (nodeV1Api)
import           Pos.Chain.Txp (Utxo)
import           Pos.Node.API (ForceNtpCheck, NodeInfo, NodeSettings)
import           Pos.Util.Jsend (ResponseStatus (..))
import           Pos.Util.Servant (APIResponse (..))
import           Pos.Web.Types (CConfirmedProposalState)


-- * Node Client

data NodeClient m
    = NodeClient
    { getUtxo
        :: m Utxo

    , getConfirmedProposals
        :: m [CConfirmedProposalState]

    , getNodeSettings
        :: m NodeSettings

    , getNodeInfo
        :: ForceNtpCheck
        -> m NodeInfo

    , applyUpdate
        :: m ()

    , postponeUpdate
        :: m ()
    } deriving (Generic)


data ClientError a
    = KnownError a
    | DecodeFailure Text Response
    | UnsupportedContentType MediaType Response
    | InvalidContentTypeHeader Response
    | ConnectionError Text
    deriving (Show, Generic, Eq)
instance Exception a => Exception (ClientError a)

fromServantError :: FromJSON a => ServantError -> ClientError a
fromServantError = \case
    Servant.FailureResponse r@(Response _ _ _ body) ->
        case Aeson.decode body of
            Just (APIResponse a ErrorStatus _) ->
                KnownError a
            Just _ ->
                DecodeFailure "API failed with non-error response ?!?" r
            Nothing ->
                DecodeFailure "Invalid / Non-JSEnd API Error Response" r
    Servant.DecodeFailure t r ->
        DecodeFailure t r
    Servant.UnsupportedContentType m r ->
        UnsupportedContentType m r
    Servant.InvalidContentTypeHeader r ->
        InvalidContentTypeHeader r
    Servant.ConnectionError t ->
        ConnectionError t


-- * HTTP Instance

type NodeHttpClient = NodeClient (ExceptT (ClientError ()) IO)

mkHttpClient
    :: BaseUrl
    -> Manager
    -> NodeHttpClient
mkHttpClient baseUrl manager = NodeClient
    { getUtxo =
        run getUtxoR
    , getConfirmedProposals =
        run getConfirmedProposalsR
    , getNodeSettings =
        fmap wrData $ run getNodeSettingsR
    , getNodeInfo =
        fmap wrData . run . getNodeInfoR
    , applyUpdate =
        void $ run applyUpdateR
    , postponeUpdate =
        void $ run postponeUpdateR
    }
  where
    run :: forall a. ClientM a -> ExceptT (ClientError ()) IO a
    run = ExceptT
        . fmap (first fromServantError)
        . flip runClientM (ClientEnv manager baseUrl noCookieJar)

    noCookieJar = Nothing

    (       getNodeSettingsR
     :<|>   getNodeInfoR
     :<|>   applyUpdateR
     :<|>   postponeUpdateR
     ):<|>( getUtxoR
     :<|>   getConfirmedProposalsR
     ) = client nodeV1Api
