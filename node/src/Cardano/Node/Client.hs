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
    -- * Deprecated
    , applyUpdate
    , postponeUpdate
    ) where

import           Universum

import           Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import           Network.HTTP.Client (Manager)
import           Network.HTTP.Types (status404)
import           Servant ((:<|>) (..))
import           Servant.Client (BaseUrl (..), ClientEnv (..), ClientM,
                     GenResponse (..), ServantError, client, runClientM)
import qualified Servant.Client as Servant

import           Cardano.Node.API (nodeV1Api)
import           Pos.Chain.Txp (Utxo)
import qualified Pos.Chain.Update as Core
import           Pos.Node.API (ForceNtpCheck, NodeInfo, NodeSettings, V1)
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

    , restartNode
        :: m ()

    , getNextUpdate
        :: m (V1 Core.SoftwareVersion)
    } deriving (Generic)


-- | A backwards compatibility wrapper for 'restartNode'.
applyUpdate :: NodeClient m -> m ()
applyUpdate = restartNode
{-# DEPRECATED applyUpdate "Use 'restartNode' instead." #-}

-- | 'postponeUpdate' was removed from the API. This is a backwards
-- compatibility wrapper that is deprecated.
postponeUpdate :: Applicative m => NodeClient n -> m ()
postponeUpdate _ = pure ()
{-# DEPRECATED postponeUpdate "This endpoint was turned into a noop." #-}

data ClientError a
    = KnownError a
    | ErrFromServant Servant.ServantError
    deriving (Show, Generic, Eq)

instance Exception a => Exception (ClientError a)

fromServantError :: FromJSON a => ServantError -> ClientError a
fromServantError err = case err of
    Servant.FailureResponse r@(Response s _ _ body)
        | s == status404 ->
            ErrFromServant err
        | otherwise ->
            case Aeson.decode body of
                Just (APIResponse a ErrorStatus _) ->
                    KnownError a
                Just _ ->
                    ErrFromServant $ Servant.DecodeFailure "API failed with non-error response ?!?" r
                Nothing ->
                    ErrFromServant $ Servant.DecodeFailure "Invalid / Non-JSEnd API Error Response" r
    _ ->
        ErrFromServant err

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
    , getNextUpdate =
        wrData <$> run getNextUpdateR
    , restartNode =
        void $ run restartNodeR
    }
  where
    run :: forall a. ClientM a -> ExceptT (ClientError ()) IO a
    run = ExceptT
        . fmap (first fromServantError)
        . flip runClientM (ClientEnv manager baseUrl noCookieJar)

    noCookieJar = Nothing

    (       getNodeSettingsR
     :<|>   getNodeInfoR
     :<|>   getNextUpdateR
     :<|>   restartNodeR
     ):<|>( getUtxoR
     :<|>   getConfirmedProposalsR
     ) = client nodeV1Api
