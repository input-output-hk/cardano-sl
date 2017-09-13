{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Some utilites for more flexible servant usage.

module Pos.Util.Servant
    ( ModifiesApiRes (..)
    , VerbMod

    , ReportDecodeError (..)
    , CDecodeApiArg

    , OriginType
    , FromCType (..)
    , ToCType (..)

    , WithDefaultApiArg

    , HasLoggingServer (..)
    , ApiLoggingConfig
    , LoggingApi
    , LoggingApiRec

    , CQueryParam
    , CCapture
    , CReqBody
    , DCQueryParam

    , serverHandlerL
    , serverHandlerL'
    , inRouteServer

    , applyLoggingToVerb
    ) where

import           Universum

import           Control.Lens            (Iso, iso, makePrisms)
import           Control.Monad.Catch     (handleAll)
import           Control.Monad.Except    (ExceptT (..), MonadError (..))
import           Data.Default            (Default (..))
import           Data.Reflection         (Reifies (..), reflect)
import           Formatting              (bprint, build, formatToString, sformat, shown,
                                          stext, string, (%))
import           Serokell.Util.ANSI      (Color (..))
import           Servant.API             ((:<|>) (..), (:>), Capture, QueryParam,
                                          ReflectMethod (..), ReqBody, Verb)
import           Servant.Server          (Handler (..), HasServer (..), ServantErr,
                                          Server)
import qualified Servant.Server.Internal as SI
import           System.Wlog             (LoggerName, logInfo, usingLoggerName)

import           Pos.Util.Util           (colorizeDull)

-------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------

serverHandlerL :: Iso (Handler a) (Handler b) (ExceptT ServantErr IO a) (ExceptT ServantErr IO b)
serverHandlerL = iso runHandler' Handler

serverHandlerL' :: Iso (Handler a) (Handler b) (IO $ Either ServantErr a) (IO $ Either ServantErr b)
serverHandlerL' = serverHandlerL . iso runExceptT ExceptT

inRouteServer
    :: forall api api' ctx env.
       (Proxy api -> SI.Context ctx -> SI.Delayed env (Server api) -> SI.Router env)
    -> (Server api' -> Server api)
    -> (Proxy api' -> SI.Context ctx -> SI.Delayed env (Server api') -> SI.Router env)
inRouteServer routing f = \_ ctx delayed -> routing Proxy ctx (fmap f delayed)

-------------------------------------------------------------------------
-- General useful families
-------------------------------------------------------------------------

-- | Proves info about argument specifier of servant API.
class ApiHasArgClass apiType a where
    -- | For arguments-specifiers of API, get argument type.
    type ApiArg (apiType :: * -> *) a :: *
    type ApiArg apiType a = a

    -- | Name of argument.
    -- E.g. name of argument specified by @Capture "nyan"@ is /nyan/.
    apiArgName :: Proxy (apiType a) -> String
    default apiArgName
        :: forall n someApiType. (KnownSymbol n, someApiType n ~ apiType)
        => Proxy (someApiType n a) -> String
    apiArgName _ = formatToString ("'"%string%"' field") $ symbolVal (Proxy @n)

type ApiHasArgInvariant apiType a res =
    Server (apiType a :> res) ~ (ApiArg apiType a -> Server res)

type ApiHasArg apiType a res =
    ( ApiHasArgClass apiType a
    , ApiHasArgInvariant apiType a res
    )

instance KnownSymbol s => ApiHasArgClass (Capture s) a
instance KnownSymbol s => ApiHasArgClass (QueryParam s) a where
    type ApiArg (QueryParam s) a = Maybe a
instance ApiHasArgClass (ReqBody ct) a where
    apiArgName _ = "request body"

-------------------------------------------------------------------------
-- Mapping API result
-------------------------------------------------------------------------

-- | Modifies result of API.
class ModifiesApiRes t where
    type ApiModifiedRes t a :: *
    modifyApiResult
        :: Proxy t
        -> IO (Either ServantErr a)
        -> IO (Either ServantErr (ApiModifiedRes t a))

-- | Wrapper to modify result of API.
-- @modType@ argument specifies transformation and should be instance of
-- 'ModifiesApiRes'.
data VerbMod (modType :: *) api

instance (HasServer (Verb mt st ct $ ApiModifiedRes mod a) ctx,
          ModifiesApiRes mod) =>
         HasServer (VerbMod mod $ Verb (mt :: k1) (st :: Nat) (ct :: [*]) a) ctx where
    type ServerT (VerbMod mod $ Verb mt st ct a) m =
         ServerT (Verb mt st ct a) m

    route = inRouteServer @(Verb mt st ct $ ApiModifiedRes mod a) route $
            serverHandlerL' %~ modifyApiResult (Proxy @mod)

-------------------------------------------------------------------------
-- Mapping API arguments: client types decoding
-------------------------------------------------------------------------

-- | For many types with nice structure there exists a /client type/, which is
-- an intermediate representation between internal types and JSON. Their ToJSON
-- instances are derived automatically and they are used by daedalus-bridge.
-- | This family maps /client types/ to their respective original types
-- (e.g. @CAccountAddress@ -> @AccountAddress@).
type family OriginType ctype :: *

class FromCType c where
   -- | Way to decode from @CType@.
   decodeCType :: c -> Either Text (OriginType c)

class ToCType c where
   -- | Way to encode to @CType@.
   encodeCType :: OriginType c -> c

type instance OriginType (Maybe a) = Maybe $ OriginType a

instance FromCType a => FromCType (Maybe a) where
    decodeCType = mapM decodeCType


-- | Allows to throw error from anywhere within the internals of Servant API.
class ReportDecodeError api where
    -- | Propagate error to servant handler.
    reportDecodeError :: Proxy api -> Text -> Server api

instance ( ReportDecodeError res
         , ApiHasArg argType a res
         ) =>
         ReportDecodeError (argType a :> res) where
    reportDecodeError _ err = \_ -> reportDecodeError (Proxy @res) err


-- | Wrapper over API argument specifier which says to decode specified argument
-- with 'decodeCType'.
data CDecodeApiArg (argType :: * -> *) a

instance ApiHasArgClass apiType a =>
         ApiHasArgClass (CDecodeApiArg apiType) a where
    type ApiArg (CDecodeApiArg apiType) a = OriginType (ApiArg apiType a)
    apiArgName _ = apiArgName (Proxy @(apiType a))

instance ( HasServer (apiType a :> res) ctx
         , ApiHasArg apiType a res
         , FromCType (ApiArg apiType a)
         , ReportDecodeError res
         ) =>
         HasServer (CDecodeApiArg apiType a :> res) ctx where
    type ServerT (CDecodeApiArg apiType a :> res) m =
         OriginType (ApiArg apiType a) -> ServerT res m
    route =
        inRouteServer @(apiType a :> res) route $
        \f a -> either reportE f $ decodeCType a
      where
        reportE err =
            reportDecodeError (Proxy @res) $
            sformat ("(in "%string%") "%stext)
                (apiArgName $ Proxy @(apiType a))
                err

-------------------------------------------------------------------------
-- Mapping API arguments: defaults
-------------------------------------------------------------------------

type family UnmaybeArg a where
    UnmaybeArg (Maybe a -> b) = a -> b

type family Unmaybe a where
    Unmaybe (Maybe a) = a

data WithDefaultApiArg (argType :: * -> *) a

instance (ApiHasArgClass apiType a, ApiArg apiType a ~ Maybe b) =>
         ApiHasArgClass (WithDefaultApiArg apiType) a where
    type ApiArg (WithDefaultApiArg apiType) a = Unmaybe (ApiArg apiType a)
    apiArgName _ = apiArgName (Proxy @(apiType a))

instance ( HasServer (apiType a :> res) ctx
         , Server (apiType a :> res) ~ (Maybe c -> d)
         , Default c
         ) =>
         HasServer (WithDefaultApiArg apiType a :> res) ctx where
    type ServerT (WithDefaultApiArg apiType a :> res) m =
         UnmaybeArg (ServerT (apiType a :> res) m)
    route =
        inRouteServer @(apiType a :> res) route $
        \f a -> f $ fromMaybe def a

-------------------------------------------------------------------------
-- Logging
-------------------------------------------------------------------------

data LoggingApi config api
data LoggingApiRec config api
type ApiLoggingConfig = LoggerName
data ApiParamsLogInfo
    = ApiParamsLogInfo [Text]  -- ^ Parameters gathered at current stage
    | ApiNoParamsLogInfo Text  -- ^ Parameters collection failed with reason
                               --   (e.g. decoding error)

makePrisms ''ApiParamsLogInfo

instance Default ApiParamsLogInfo where
    def = ApiParamsLogInfo mempty

instance HasServer (LoggingApiRec config api) ctx =>
         HasServer (LoggingApi config api) ctx where
    type ServerT (LoggingApi config api) m = ServerT api m
    route = inRouteServer @(LoggingApiRec config api) route
            (def, )

class HasLoggingServer config api ctx where
    routeWithLog
        :: Proxy (LoggingApiRec config api)
        -> SI.Context ctx
        -> SI.Delayed env (Server (LoggingApiRec config api))
        -> SI.Router env

instance HasLoggingServer config api ctx =>
         HasServer (LoggingApiRec config api) ctx where
    type ServerT (LoggingApiRec config api) m =
         (ApiParamsLogInfo, ServerT api m)
    route = routeWithLog

instance ( HasLoggingServer config api1 ctx
         , HasLoggingServer config api2 ctx
         ) =>
         HasLoggingServer config (api1 :<|> api2) ctx where
    routeWithLog =
        inRouteServer
            @(LoggingApiRec config api1 :<|> LoggingApiRec config api2)
            route $
            \(paramsInfo, f1 :<|> f2) -> (paramsInfo, f1) :<|> (paramsInfo, f2)

instance ( KnownSymbol path
         , HasLoggingServer config res ctx
         ) =>
         HasLoggingServer config (path :> res) ctx where
    routeWithLog =
        inRouteServer @(path :> LoggingApiRec config res) route $
        \(paramsInfo, f) -> (updateParamsInfo paramsInfo, f)
      where
        updateParamsInfo = do
            let path = toText . symbolVal $ Proxy @path
            _ApiParamsLogInfo %~ (path :)

class ApiHasArgClass apiType a =>
      ApiCanLogArg apiType a where
    type ApiArgToLog (apiType :: * -> *) a :: *
    type ApiArgToLog apiType a = a

    toLogParamInfo
        :: Show (ApiArgToLog apiType a)
        => Proxy (apiType a) -> ApiArg apiType a -> Text
    default toLogParamInfo
        :: (Show a, ApiArgToLog apiType a ~ a)
        => Proxy (apiType a) -> a -> Text
    toLogParamInfo _ = show

instance KnownSymbol s => ApiCanLogArg (Capture s) a
instance ApiCanLogArg (ReqBody ct) a
instance KnownSymbol cs => ApiCanLogArg (QueryParam cs) a where
    toLogParamInfo _ = maybe noEntry show
      where
        noEntry = colorizeDull White "-"

instance ( ApiHasArgClass apiType a
         , ApiArg apiType a ~ Maybe b
         , ApiCanLogArg apiType a
         , Show (ApiArgToLog apiType a)
         ) =>
         ApiCanLogArg (WithDefaultApiArg apiType) a where
    type ApiArgToLog (WithDefaultApiArg apiType) a = Unmaybe (ApiArg apiType a)
    toLogParamInfo _ = show

instance ( ApiCanLogArg apiType a
         , Show (ApiArgToLog apiType a)
         ) =>
         ApiCanLogArg (CDecodeApiArg apiType) a where
    type ApiArgToLog (CDecodeApiArg apiType) a = OriginType (ApiArg apiType a)
    toLogParamInfo _ = show

instance ( HasServer (apiType a :> LoggingApiRec config res) ctx
         , ApiHasArg apiType a res
         , ApiHasArg apiType a (LoggingApiRec config res)
         , ApiCanLogArg apiType a
         , Show (ApiArgToLog apiType a)
         ) =>
         HasLoggingServer config (apiType a :> res) ctx where
    routeWithLog =
        inRouteServer @(apiType a :> LoggingApiRec config res) route $
        \(paramsInfo, f) a -> (a `updateParamsInfo` paramsInfo, f a)
      where
        updateParamsInfo a = do
            let paramName = apiArgName $ Proxy @(apiType a)
                paramVal  = toLogParamInfo (Proxy @(apiType a)) a
                paramInfo = sformat (string%": "%stext) paramName paramVal
            _ApiParamsLogInfo %~ (paramInfo :)

applyLoggingToVerb
    :: ( MonadIO m
       , MonadCatch m
       , MonadError ServantErr m
       , Reifies config ApiLoggingConfig
       )
    => Proxy config -> Text -> ApiParamsLogInfo -> (a -> Text) -> m a -> m a
applyLoggingToVerb configP method paramsInfo showResponse action =
    catchErrors $ do
        res <- action
        reportResponse res
        return res
  where
    doLog msg = do
        let loggerName = reflect configP
        liftIO . usingLoggerName loggerName $ logInfo msg
    mkParamLogs = case paramsInfo of
        (ApiParamsLogInfo info) -> do
            let params = mconcat $ reverse info <&>
                  bprint ("    "%stext%" "%stext%"\n") (colorizeDull White ":>")
            Right $ sformat ("\n"%stext%"\n"%build) cmethod params
        ApiNoParamsLogInfo why -> Left why
    logWithParamInfo msg =
        case mkParamLogs of
            Left e ->
                doLog $ sformat ("\n"%stext%" "%stext)
                        (colorizeDull Red "Unexecuted request due to error") e
            Right paramLogs ->
                doLog (paramLogs <> msg)
    reportResponse resp =
        logWithParamInfo $
            sformat (stext%" "%stext%"\n"%stext)
                (colorizeDull White "Status")
                (colorizeDull Green "OK")
                (showResponse resp)
    catchErrors =
        flip catchError servantErrHandler .
        handleAll totalHandler
    servantErrHandler (e :: ServantErr) = do
        logWithParamInfo $
            sformat ("Status: "%stext%"\n") (colorizeDull Red $ show e)
        throwError e
    totalHandler (e :: SomeException) = do
        logWithParamInfo $
            sformat (stext%" "%shown%"\n") (colorizeDull Red "Exception") e
        throwM e
    cmethod = flip colorizeDull method $ case method of
        "GET"    -> Cyan
        "POST"   -> Yellow
        "PUT"    -> Blue
        "DELETE" -> Red
        _        -> Magenta

instance ( HasServer (Verb mt st ct a) ctx
         , Reifies config ApiLoggingConfig
         , ReflectMethod mt
         , Show a
         ) =>
         HasLoggingServer config (Verb (mt :: k1) (st :: Nat) (ct :: [*]) a) ctx where
    routeWithLog =
        inRouteServer @(Verb mt st ct a) route $
        \(paramsInfo, handler) ->
            handler & serverHandlerL %~ withLogging paramsInfo
      where
        method = decodeUtf8 $ reflectMethod (Proxy @mt)
        withLogging params = applyLoggingToVerb (Proxy @config) method params show

instance ReportDecodeError api =>
         ReportDecodeError (LoggingApiRec config api) where
    reportDecodeError _ msg =
        (ApiNoParamsLogInfo msg, reportDecodeError (Proxy @api) msg)

-------------------------------------------------------------------------
-- API construction Helpers
-------------------------------------------------------------------------

type CQueryParam s a = CDecodeApiArg (QueryParam s) a
type CCapture s a    = CDecodeApiArg (Capture s) a
type CReqBody c a    = CDecodeApiArg (ReqBody c) a

type DCQueryParam s a = WithDefaultApiArg (CDecodeApiArg $ QueryParam s) a
