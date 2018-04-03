{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

-- | Some utilites for more flexible servant usage.

module Pos.Util.Servant
    ( ApiHasArgClass (..)
    , ApiCanLogArg (..)


    , ModifiesApiRes (..)
    , VerbMod

    , ReportDecodeError (..)
    , CDecodeApiArg

    , OriginType
    , FromCType (..)
    , ToCType (..)

    , WithDefaultApiArg

    , HasLoggingServer (..)
    , ApiLoggingConfig (..)
    , ApiParamsLogInfo (..)
    , LoggingApi
    , LoggingApiRec
    , WithTruncatedLog (..)
    , HasTruncateLogPolicy (..)
    , _ApiParamsLogInfo
    , _ApiNoParamsLogInfo
    , addParamLogInfo

    , CQueryParam
    , CCapture
    , CReqBody
    , DReqBody
    , DCQueryParam
    , DQueryParam

    , serverHandlerL
    , serverHandlerL'
    , inRouteServer

    , applyLoggingToHandler
    ) where

import           Universum hiding (id)

import           Control.Exception.Safe (handleAny)
import           Control.Lens (Iso, iso, makePrisms)
import           Control.Monad.Except (ExceptT (..), MonadError (..))
import           Data.Constraint ((\\))
import           Data.Constraint.Forall (Forall, inst)
import           Data.Default (Default (..))
import           Data.Reflection (Reifies (..), reflect)
import qualified Data.Text as T
import qualified Data.Text.Buildable
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Formatting (bprint, build, builder, fconst, formatToString, sformat, shown, stext,
                             string, (%))
import           GHC.IO.Unsafe (unsafePerformIO)
import           GHC.TypeLits (KnownSymbol, symbolVal)
import           Serokell.Util (listJsonIndent)
import           Serokell.Util.ANSI (Color (..), colorizeDull)
import           Servant.API ((:<|>) (..), (:>), Capture, Description, QueryParam,
                              ReflectMethod (..), ReqBody, Summary, Verb)
import           Servant.Client (Client, HasClient (..))
import           Servant.Client.Core (RunClient)
import           Servant.Server (Handler (..), HasServer (..), ServantErr (..), Server)
import qualified Servant.Server.Internal as SI
import           Servant.Swagger (HasSwagger (toSwagger))
import           System.Wlog (LoggerName, LoggerNameBox, usingLoggerName)

import           Pos.Util.LogSafe (SecureLog, BuildableSafe, SecuredText, buildSafe, logInfoSP,
                                   plainOrSecureF, secretOnlyF)

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

-- | Extract right side of type application.
type family ApplicationRS api where
    ApplicationRS (apiType a) = a

-- | Proves info about argument specifier of servant API.
class ApiHasArgClass api where
    -- | For arguments-specifiers of API, get argument type.
    -- E.g. @Capture "cap" Int@ -> @Int@.
    type ApiArg api :: *
    type ApiArg api = ApplicationRS api

    -- | Name of argument.
    -- E.g. name of argument specified by @Capture "nyan"@ is /nyan/.
    apiArgName
        :: Proxy api -> String
    default apiArgName
        :: forall n someApiType a. (KnownSymbol n, someApiType n a ~ api)
        => Proxy api -> String
    apiArgName _ = formatToString ("'"%string%"' field") $ symbolVal (Proxy @n)

class ServerT (subApi :> res) m ~ (ApiArg subApi -> ServerT res m)
   => ApiHasArgInvariant subApi res m
instance ServerT (subApi :> res) m ~ (ApiArg subApi -> ServerT res m)
      => ApiHasArgInvariant subApi res m

type ApiHasArg subApi res =
    ( ApiHasArgClass subApi
    , ApiHasArgInvariant subApi res Handler   -- this one for common cases
    , Forall (ApiHasArgInvariant subApi res)  -- and this is generalized one
    )

instance KnownSymbol s => ApiHasArgClass (Capture s a)
instance KnownSymbol s => ApiHasArgClass (QueryParam s a) where
    type ApiArg (QueryParam s a) = Maybe a
instance ApiHasArgClass (ReqBody ct a) where
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

instance ( HasServer (Verb mt st ct $ ApiModifiedRes mod a) ctx
         , HasServer (Verb mt st ct a) ctx
         , ModifiesApiRes mod
         ) =>
         HasServer (VerbMod mod $ Verb (mt :: k1) (st :: Nat) (ct :: [*]) a) ctx where
    type ServerT (VerbMod mod $ Verb mt st ct a) m =
         ServerT (Verb mt st ct a) m

    route = inRouteServer @(Verb mt st ct $ ApiModifiedRes mod a) route $
            serverHandlerL' %~ modifyApiResult (Proxy @mod)

    hoistServerWithContext _ pc nt s =
        hoistServerWithContext (Proxy :: Proxy (Verb mt st ct api)) pc nt s

instance HasSwagger v => HasSwagger (VerbMod mod v) where
    toSwagger _ = toSwagger $ Proxy @v

-------------------------------------------------------------------------
-- Mapping API arguments: client types decoding
-------------------------------------------------------------------------

-- | For many types with nice structure there exists a /client type/, which is
-- an intermediate representation between internal types and JSON.
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
         , ApiHasArg subApi res
         ) =>
         ReportDecodeError (subApi :> res) where
    reportDecodeError _ err = \_ -> reportDecodeError (Proxy @res) err

-- | Wrapper over API argument specifier which says to decode specified argument
-- with 'decodeCType'.
data CDecodeApiArg subApi

instance ApiHasArgClass subApi =>
         ApiHasArgClass (CDecodeApiArg subApi) where
    type ApiArg (CDecodeApiArg subApi) = OriginType (ApiArg subApi)
    apiArgName _ = apiArgName (Proxy @subApi)

instance ( HasServer (subApi :> res) ctx
         , HasServer res ctx
         , ApiHasArg subApi res
         , FromCType (ApiArg subApi)
         , ReportDecodeError res
         ) =>
         HasServer (CDecodeApiArg subApi :> res) ctx where
    type ServerT (CDecodeApiArg subApi :> res) m =
         OriginType (ApiArg subApi) -> ServerT res m

    route =
        inRouteServer @(subApi :> res) route $
        \f a -> either reportE f $ decodeCType a
      where
        reportE err =
            reportDecodeError (Proxy @res) $
            sformat ("(in "%string%") "%stext)
                (apiArgName $ Proxy @subApi)
                err

    hoistServerWithContext _ pc nt s =
            hoistServerWithContext (Proxy @res) pc nt . s

instance HasSwagger (subApi :> res) =>
         HasSwagger (CDecodeApiArg subApi :> res) where
    toSwagger _ = toSwagger (Proxy @(subApi :> res))

-------------------------------------------------------------------------
-- Mapping API arguments: defaults
-------------------------------------------------------------------------

type family UnmaybeArg a where
    UnmaybeArg (Maybe a -> b) = a -> b

type family Unmaybe a where
    Unmaybe (Maybe a) = a

data WithDefaultApiArg subApi

instance (ApiHasArgClass subApi, ApiArg subApi ~ Maybe b) =>
         ApiHasArgClass (WithDefaultApiArg subApi) where
    type ApiArg (WithDefaultApiArg subApi) = Unmaybe (ApiArg subApi)
    apiArgName _ = apiArgName (Proxy @subApi)

instance ( HasServer (subApi :> res) ctx
         , HasServer res ctx
         , ApiHasArg (WithDefaultApiArg subApi) res
         , Server (subApi :> res) ~ (Maybe c -> d)
         , Default c
         ) =>
         HasServer (WithDefaultApiArg subApi :> res) ctx where
    type ServerT (WithDefaultApiArg subApi :> res) m =
         UnmaybeArg (ServerT (subApi :> res) m)

    route =
        inRouteServer @(subApi :> res) route $
        \f a -> f $ fromMaybe def a

    -- The simpliest what we can do here is to delegate to
    -- @hoistServer (Proxy @res)@.
    -- However to perform that we need a knowledge of that
    -- @ServerT (withDefaultApiArg subApi :> res) m@ is a /function/
    -- regardless of @m@.
    -- I.e. we have to set a constraint
    -- @forall m. ServerT (WithDefaultApiArg apiType :> res) m ~ (e -> d)@
    -- (for some @e@ and @d@).
    -- However GHC doesn't allow @forall@ in constraints.
    --
    -- But we can use 'Forall' thing instead.
    -- @Forall (ApiHasArgInvariant (WithDefaultApiArg apiType) a res)@
    -- constraint is already provided here (see 'ApiHasArg' definition),
    -- so we can instantiate it's quantification parameter @m@ to concrete
    -- @m1@ and @m2@ required by this function, getting desired constraints.
    hoistServerWithContext _ pc (nt :: forall x. m1 x -> m2 x) s =
        hoistServerWithContext (Proxy @res) pc nt . s
        \\ inst @(ApiHasArgInvariant (WithDefaultApiArg subApi) res) @m1
        \\ inst @(ApiHasArgInvariant (WithDefaultApiArg subApi) res) @m2

instance HasSwagger (subApi :> res) =>
    HasSwagger (WithDefaultApiArg subApi :> res) where
    toSwagger _ = toSwagger (Proxy @(subApi :> res))

-------------------------------------------------------------------------
-- HasClient instances we need for benchmarking.
-------------------------------------------------------------------------

instance HasClient m (subApi :> res) => HasClient m (CDecodeApiArg subApi :> res) where
    type Client m (CDecodeApiArg subApi :> res) = Client m (subApi :> res)
    clientWithRoute p _ req = clientWithRoute p (Proxy @(subApi :> res)) req

instance HasClient m (subApi :> res) =>
         HasClient m (WithDefaultApiArg subApi :> res) where
    type Client m (WithDefaultApiArg subApi :> res) = Client m (subApi :> res)
    clientWithRoute p _ req = clientWithRoute p (Proxy @(subApi :> res)) req

instance (RunClient m, HasClient m (Verb mt st ct $ ApiModifiedRes mod a)) =>
         HasClient m (VerbMod mod (Verb (mt :: k1) (st :: Nat) (ct :: [*]) a)) where
    type Client m (VerbMod mod (Verb mt st ct a)) = Client m (Verb mt st ct $ ApiModifiedRes mod a)
    clientWithRoute p _ req =
        clientWithRoute p (Proxy @(Verb mt st ct $ ApiModifiedRes mod a)) req

-------------------------------------------------------------------------
-- Logging
-------------------------------------------------------------------------

-- | Enables logging for server which serves given api.
--
-- `config` is a type at which you have to specify 'ApiLoggingConfig' via
-- reflection. This way was chosen because the least thing we need in
-- config is 'LoggerName', and we want to have '<>' on 'LoggerName's thus
-- 'KnownSymbol' is not enough.
--
-- This logging will report
--
-- * Request parameters, including request bodies
-- * Truncated response (for exact meaning of /truncated/ see 'WithTruncatedLog')
-- * If execution failed with error, it will be displayed
-- * Details like request method and endpoint execution time
--
-- If user makes request which can't be processed (e.g. with path to undefined
-- endpoint which normally terminates with 404) it won't be logged. However,
-- I don't find it a great problem, it may impede only in development or on
-- getting acknowledged with api.
data LoggingApi config api

-- | Helper to traverse servant api and apply logging.
data LoggingApiRec config api

newtype ApiLoggingConfig = ApiLoggingConfig
    { apiLoggerName :: LoggerName
    } deriving Show

-- | Used to incrementally collect info about passed parameters.
data ApiParamsLogInfo
      -- | Parameters gathered at current stage
    = ApiParamsLogInfo [SecuredText]
      -- | Parameters collection failed with reason
      --   (e.g. decoding error)
    | ApiNoParamsLogInfo Text

makePrisms ''ApiParamsLogInfo

instance Default ApiParamsLogInfo where
    def = ApiParamsLogInfo mempty

addParamLogInfo :: SecuredText -> ApiParamsLogInfo -> ApiParamsLogInfo
addParamLogInfo paramInfo = _ApiParamsLogInfo %~ (paramInfo :)

-- | When it comes to logging responses, returned data may be very large.
-- Log space is valuable (already in testnet we got truncated logs),
-- so we have to care about printing only whose data which may be useful.
newtype WithTruncatedLog a = WithTruncatedLog a

instance {-# OVERLAPPABLE #-}
         Buildable a => Buildable (WithTruncatedLog a) where
    build (WithTruncatedLog a) = bprint build a

-- | When item list is going to be printed, we impose taking care of
-- truncating.
-- How much data to remain should depend on how big output may be, how can
-- response change from call to call and how often related endpoints are called
-- in practise.
class HasTruncateLogPolicy a where
    truncateLogPolicy :: [a] -> [a]

instance (Buildable (WithTruncatedLog a), HasTruncateLogPolicy a) =>
         Buildable (WithTruncatedLog [a]) where
    build (WithTruncatedLog l) = do
        let lt = truncateLogPolicy l
            diff = length l - length lt
            mMore | diff == 0 = ""
                  | otherwise = bprint ("\n    and "%build%" entries more...")
                                diff
        bprint (listJsonIndent 4%builder)
            (map WithTruncatedLog lt)
            mMore


instance ( HasServer (LoggingApiRec config api) ctx
         , HasServer api ctx
         ) =>
         HasServer (LoggingApi config api) ctx where
    type ServerT (LoggingApi config api) m = ServerT api m

    route = inRouteServer @(LoggingApiRec config api) route
            (def, )

    hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy api)

-- | Version of 'HasServer' which is assumed to perform logging.
-- It's helpful because 'ServerT (LoggingApi ...)' is already defined for us
-- in actual 'HasServer' instance once and forever.
class HasServer api ctx => HasLoggingServer config api ctx where
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

    hoistServerWithContext _ pc nt s =
        hoistServerWithContext (Proxy :: Proxy api) pc nt <$> s

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
        first updateParamsInfo
      where
        updateParamsInfo =
            let path = const . toText . symbolVal $ Proxy @path
            in  addParamLogInfo path

-- | Describes a way to log a single parameter.
class ApiHasArgClass subApi =>
      ApiCanLogArg subApi where
    type ApiArgToLog subApi :: *
    type ApiArgToLog subApi = ApiArg subApi

    toLogParamInfo
        :: BuildableSafe (ApiArgToLog subApi)
        => Proxy subApi -> ApiArg subApi -> SecuredText
    default toLogParamInfo
        :: ( BuildableSafe (ApiArgToLog subApi)
           , Buildable (ApiArg subApi)
           , Buildable (SecureLog (ApiArg subApi))
           )
        => Proxy subApi -> ApiArg subApi -> SecuredText
    toLogParamInfo _ param = \sl -> sformat (buildSafe sl) param

instance KnownSymbol s => ApiCanLogArg (Capture s a)

instance ApiCanLogArg (ReqBody ct a)

instance KnownSymbol cs => ApiCanLogArg (QueryParam cs a) where
    type ApiArgToLog (QueryParam cs a) = a
    toLogParamInfo _ mparam =
        \sl -> maybe noEntry (sformat $ buildSafe sl) mparam
      where
        noEntry = colorizeDull White "-"

instance ( ApiHasArgClass subApi
         , ApiArg subApi ~ Maybe b
         , ApiCanLogArg subApi
         ) =>
         ApiCanLogArg (WithDefaultApiArg subApi) where

instance ( ApiCanLogArg subApi ) =>
         ApiCanLogArg (CDecodeApiArg subApi) where

paramRouteWithLog
    :: forall config api subApi res ctx env.
       ( api ~ (subApi :> res)
       , HasServer (subApi :> res) ctx
       , HasServer (subApi :> LoggingApiRec config res) ctx
       , ApiHasArg subApi res
       , ApiHasArg subApi (LoggingApiRec config res)
       , ApiCanLogArg subApi
       , BuildableSafe (ApiArgToLog subApi)
       )
    => Proxy (LoggingApiRec config api)
    -> SI.Context ctx
    -> SI.Delayed env (Server (LoggingApiRec config api))
    -> SI.Router env
paramRouteWithLog =
    inRouteServer @(subApi :> LoggingApiRec config res) route $
        \(paramsInfo, f) a -> (a `updateParamsInfo` paramsInfo, f a)
  where
    updateParamsInfo a =
        let paramVal = toLogParamInfo (Proxy @subApi) a
            paramName = apiArgName $ Proxy @subApi
            paramInfo =
                \sl -> sformat (string%": "%stext) paramName (paramVal sl)
        in addParamLogInfo paramInfo

instance ( HasServer (subApi :> res) ctx
         , HasServer (subApi :> LoggingApiRec config res) ctx
         , ApiHasArg subApi res
         , ApiHasArg subApi (LoggingApiRec config res)
         , ApiCanLogArg subApi
         , BuildableSafe (ApiArgToLog subApi)
         , subApi ~ apiType a
         ) =>
         HasLoggingServer config (apiType a :> res) ctx where
    routeWithLog = paramRouteWithLog

instance HasLoggingServer config res ctx =>
         HasLoggingServer config (Summary s :> res) ctx where
    routeWithLog = inRouteServer @(Summary s :> LoggingApiRec config res) route identity

instance HasLoggingServer config res ctx =>
         HasLoggingServer config (Description d :> res) ctx where
    routeWithLog = inRouteServer @(Description d :> LoggingApiRec config res) route identity


-- | Unique identifier for request-response pair.
newtype RequestId = RequestId Integer

instance Buildable RequestId where
    build (RequestId id) = bprint ("#"%build) id

-- | We want all servant servers to have non-overlapping ids,
-- so using singleton counter here.
requestsCounter :: TVar Integer
requestsCounter = unsafePerformIO $ newTVarIO 0
{-# NOINLINE requestsCounter #-}

nextRequestId :: MonadIO m => m RequestId
nextRequestId = atomically $ do
    modifyTVar' requestsCounter (+1)
    RequestId <$> readTVar requestsCounter

-- | Modify an action so that it performs all the required logging.
applyServantLogging
    :: ( MonadIO m
       , MonadCatch m
       , MonadError ServantErr m
       , Reifies config ApiLoggingConfig
       , ReflectMethod (method :: k)
       )
    => Proxy config
    -> Proxy method
    -> ApiParamsLogInfo
    -> (a -> Text)
    -> m a
    -> m a
applyServantLogging configP methodP paramsInfo showResponse action = do
    timer <- mkTimer
    reqId <- nextRequestId
    catchErrors reqId timer $ do
        reportRequest reqId
        res <- action
        reportResponse reqId timer res
        return res
  where
    method = decodeUtf8 $ reflectMethod methodP
    cmethod =
        flip colorizeDull method $
            case method of
            "GET"    -> Cyan
            "POST"   -> Yellow
            "PUT"    -> Blue
            "DELETE" -> Red
            _        -> Magenta
    mkTimer :: MonadIO m => m (m Text)
    mkTimer = do
        startTime <- liftIO getPOSIXTime
        return $ do
            endTime <- liftIO getPOSIXTime
            return $ sformat shown (endTime - startTime)
    inLogCtx :: MonadIO m => LoggerNameBox m a -> m a
    inLogCtx logAction = do
        let ApiLoggingConfig{..} = reflect configP
        usingLoggerName apiLoggerName logAction
    eParamLogs :: Either Text SecuredText
    eParamLogs = case paramsInfo of
        ApiParamsLogInfo info -> Right $ \sl ->
            T.intercalate "\n" $ reverse info <&> \securedParamsInfo ->
                sformat ("    "%stext%" "%stext)
                    (colorizeDull White ":>")
                    (securedParamsInfo sl)
        ApiNoParamsLogInfo why -> Left why
    reportRequest :: MonadIO m => RequestId -> m ()
    reportRequest reqId =
        case eParamLogs of
            Left e ->
                inLogCtx $ logInfoSP $ \sl ->
                    sformat ("\n"%stext%secretOnlyF sl (" "%stext))
                        (colorizeDull Red "Unexecuted request due to error") e
            Right paramLogs -> do
                inLogCtx $ logInfoSP $ \sl ->
                    sformat ("\n"%stext%" "%stext%"\n"%build)
                        cmethod
                        (colorizeDull White $ "Request " <> pretty reqId)
                        (paramLogs sl)
    responseTag reqId = "Response " <> pretty reqId
    reportResponse reqId timer resp = do
        durationText <- timer
        inLogCtx $ logInfoSP $ \sl ->
            sformat ("\n    "%stext%" "%stext%" "%stext
                    %plainOrSecureF sl (stext%stext) (fconst ""%fconst ""))
                (colorizeDull White $ responseTag reqId)
                (colorizeDull Green "OK")
                durationText
                (colorizeDull White " > ")
                (showResponse resp)
    catchErrors reqId st =
        flip catchError (servantErrHandler reqId st) .
        handleAny (exceptionsHandler reqId st)
    servantErrHandler reqId timer err@ServantErr{..} = do
        durationText <- timer
        let errMsg = sformat (build%" "%string) errHTTPCode errReasonPhrase
        inLogCtx $ logInfoSP $ \_sl ->
            sformat ("\n    "%stext%" "%stext%" "%stext)
                (colorizeDull White $ responseTag reqId)
                (colorizeDull Red errMsg)
                durationText
        throwError err
    exceptionsHandler reqId timer e = do
        durationText <- timer
        inLogCtx $ logInfoSP $ \_sl ->
            sformat ("\n    "%stext%" "%shown%" "%stext)
                (colorizeDull Red $ responseTag reqId)
                e
                durationText
        throwM e

applyLoggingToHandler
    :: forall config method a.
       ( Buildable (WithTruncatedLog a)
       , Reifies config ApiLoggingConfig
       , ReflectMethod method
       )
    => Proxy config -> Proxy (method :: k) -> (ApiParamsLogInfo, Handler a) -> Handler a
applyLoggingToHandler configP methodP (paramsInfo, handler) =
    handler & serverHandlerL %~ withLogging paramsInfo
  where
    display = sformat build . WithTruncatedLog
    withLogging params = applyServantLogging configP methodP params display

instance ( HasServer (Verb mt st ct a) ctx
         , Reifies config ApiLoggingConfig
         , ReflectMethod mt
         , Buildable (WithTruncatedLog a)
         ) =>
         HasLoggingServer config (Verb (mt :: k) (st :: Nat) (ct :: [*]) a) ctx where
    routeWithLog =
        inRouteServer @(Verb mt st ct a) route $
        applyLoggingToHandler (Proxy @config) (Proxy @mt)

instance ( HasServer (Verb mt st ct $ ApiModifiedRes mod a) ctx
         , HasServer (VerbMod mod (Verb mt st ct a)) ctx
         , ModifiesApiRes mod
         , ReflectMethod mt
         , Reifies config ApiLoggingConfig
         , Buildable (WithTruncatedLog $ ApiModifiedRes mod a)
         ) =>
         HasLoggingServer config (VerbMod mod (Verb (mt :: k1) (st :: Nat) (ct :: [*]) a)) ctx where
    routeWithLog =
        -- TODO [CSM-466] avoid manually rewriting rule for composite api modification
        inRouteServer @(Verb mt st ct $ ApiModifiedRes mod a) route $
        \(paramsInfo, handler) ->
            handler & serverHandlerL' %~ modifyApiResult (Proxy @mod)
                    & applyLoggingToHandler (Proxy @config) (Proxy @mt) . (paramsInfo, )

instance ReportDecodeError api =>
         ReportDecodeError (LoggingApiRec config api) where
    reportDecodeError _ msg =
        (ApiNoParamsLogInfo msg, reportDecodeError (Proxy @api) msg)

-------------------------------------------------------------------------
-- API construction Helpers
-------------------------------------------------------------------------

type CQueryParam s a = CDecodeApiArg (QueryParam s a)
type CCapture s a    = CDecodeApiArg (Capture s a)
type CReqBody c a    = CDecodeApiArg (ReqBody c a)

type DReqBody c a    = WithDefaultApiArg (ReqBody c a)

type DCQueryParam s a = WithDefaultApiArg (CDecodeApiArg $ QueryParam s a)

type DQueryParam s a = WithDefaultApiArg (QueryParam s a)
