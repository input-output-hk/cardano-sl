module Test.Integration.Framework.Request
    ( HasHttpClient
    , request
    , request_
    , successfulRequest
    , unsafeRequest
    , ($-)
    ) where

import           Universum hiding (ByteString)

import           Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Generics.Product.Typed (HasType, typed)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           GHC.Exts (IsList (..))
import           Network.HTTP.Client (RequestBody (..), httpLbs, method,
                     parseRequest, requestBody, requestHeaders, responseBody,
                     responseHeaders, responseStatus, responseVersion)
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Types.Method (Method)
import           Network.HTTP.Types.Status (status200, status300, status404)
import           Servant.Client.Core.Internal.BaseUrl (BaseUrl (..),
                     showBaseUrl)
import           Servant.Client.Core.Internal.Request (ServantError (..))
import qualified Servant.Client.Core.Internal.Request as Servant

import           Cardano.Wallet.Client.Http (ClientError (..), Manager,
                     WalletClient)
import           Pos.Util.Servant (APIResponse (..))

class (HasType (WalletClient IO) ctx) => HasHttpClient ctx where
    httpClient :: Lens' ctx (WalletClient IO)
    httpClient = typed @(WalletClient IO)
instance (HasType (WalletClient IO) ctx) => HasHttpClient ctx

class (HasType (BaseUrl, Manager) ctx) => HasManager ctx where
    manager :: Lens' ctx (BaseUrl, Manager)
    manager = typed @(BaseUrl, Manager)
instance (HasType (BaseUrl, Manager) ctx) => HasManager ctx

-- | Removes some boilerplates getting the content of a 'Resp' directly.
class Request originalResponse where
    type Response originalResponse :: *

    -- | Run a given request and transform the response
    request
        :: forall m ctx. (MonadIO m, MonadReader ctx m, HasHttpClient ctx)
        => (WalletClient IO -> IO (Either ClientError originalResponse))
        -> m (Either ClientError (Response originalResponse))

    -- | Run a given request and discard the response
    request_
        :: forall m ctx. (MonadIO m, MonadReader ctx m, HasHttpClient ctx)
        => (WalletClient IO -> IO (Either ClientError originalResponse))
        -> m ()
    request_ =
        void . request

    -- | Run a given request as above, but throws if it fails
    successfulRequest
        :: forall m ctx. (MonadIO m, MonadFail m, MonadReader ctx m, HasHttpClient ctx)
        => (WalletClient IO -> IO (Either ClientError originalResponse))
        -> m (Response originalResponse)
    successfulRequest =
        request >=> \case
            Left e  ->
                fail . ("expected a successful response but got an error: " <>) . show $ e
            Right a ->
                return a

instance Request (APIResponse a) where
    type Response (APIResponse a) = a
    request action =
        view httpClient >>= liftIO . fmap (fmap wrData) . action

instance Request () where
    type Response () = ()
    request action =
        view httpClient >>= liftIO . action

unsafeRequest
    :: forall a m ctx.
        ( FromJSON a
        , MonadIO m
        , MonadThrow m
        , MonadReader ctx m
        , HasManager ctx
        )
    => (Method, Text)
    -> Maybe Aeson.Value
    -> m (Either ClientError a)
unsafeRequest (verb, path) body = do
    (base, man) <- view manager
    req <- parseRequest (showBaseUrl $ base { baseUrlPath = T.unpack path })
    res <- liftIO $ httpLbs (prepare req) man
    return $ case responseStatus res of
        s
            | s >= status200 && s <= status300 ->
                maybe
                    (Left $ decodeFailure res)
                    (Right . wrData)
                    (Aeson.decode $ responseBody res)
            | s == status404 ->
                Left
                    $ ClientHttpError
                    $ FailureResponse
                    $ Servant.Response
                    { Servant.responseStatusCode =
                        HC.responseStatus res
                    , Servant.responseHeaders =
                        Seq.fromList (HC.responseHeaders res)
                    , Servant.responseHttpVersion =
                        HC.responseVersion res
                    , Servant.responseBody =
                        HC.responseBody res
                    }

        _ -> fromMaybe (Left $ decodeFailure res) $ asum
            [ Left . ClientWalletError <$> Aeson.decode (responseBody res)
            , Left . ClientJSONError <$> Aeson.decode (responseBody res)
            ]

  where
    prepare :: HTTP.Request -> HTTP.Request
    prepare req = req
        { method = verb
        , requestBody = maybe mempty (RequestBodyLBS . Aeson.encode) body
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("Accept", "application/json")
            ]
        }

    decodeFailure :: HTTP.Response ByteString -> ClientError
    decodeFailure res = ClientHttpError $ DecodeFailure mempty $ Servant.Response
        { Servant.responseStatusCode = responseStatus res
        , Servant.responseHeaders = fromList $ responseHeaders res
        , Servant.responseHttpVersion = responseVersion res
        , Servant.responseBody = responseBody res
        }


-- | Provide "next" arguments to a function, leaving the first one untouched.
--
-- e.g.
--    myFunction  :: Ctx -> Int -> String -> Result
--    myFunction' :: Ctx -> Result
--    myFunction' = myFunction $- 14 $- "patate"
infixl 1 $-
($-) :: (a -> b -> c) -> b -> a -> c
($-) = flip
