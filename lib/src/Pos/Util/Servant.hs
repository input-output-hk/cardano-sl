{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

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
    , WithTruncatedLog (..)
    , HasTruncateLogPolicy (..)

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

import           Universum

import           Control.Exception.Safe (handleAny)
import           Control.Lens (Iso, iso, makePrisms)
import           Control.Monad.Except (ExceptT (..), MonadError (..))
import           Data.Constraint ((\\))
import           Data.Constraint.Forall (Forall, inst)
import           Data.Default (Default (..))
import           Data.Reflection (Reifies (..), reflect)
import qualified Data.Text.Buildable
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Formatting (bprint, build, builder, fconst, formatToString, sformat, shown, stext,
                             string, (%))
import           GHC.TypeLits (KnownSymbol, symbolVal)
import           Serokell.Util (listJsonIndent)
import           Serokell.Util.ANSI (Color (..), colorizeDull)
import           Servant.API ((:<|>) (..), (:>), Capture, Description, QueryParam,
                              ReflectMethod (..), ReqBody, Summary, Verb)
import           Servant.Server (Handler (..), HasServer (..), ServantErr (..), Server)
import qualified Servant.Server.Internal as SI
import           Servant.Swagger (HasSwagger (toSwagger))
import           System.Wlog (LoggerName, LoggerNameBox, usingLoggerName)

import           Pos.Util.LogSafe (BuildableSafe, SecuredText, buildSafe, logInfoSP, plainOrSecureF,
                                   secretOnlyF)

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

class ServerT (apiType a :> res) m ~ (ApiArg apiType a -> ServerT res m)
   => ApiHasArgInvariant apiType a res m
instance ServerT (apiType a :> res) m ~ (ApiArg apiType a -> ServerT res m)
      => ApiHasArgInvariant apiType a res m

type ApiHasArg apiType a res =
    ( ApiHasArgClass apiType a
    , ApiHasArgInvariant apiType a res Handler  -- this one for common cases
    , Forall (ApiHasArgInvariant apiType a res)  -- and this is generalized one
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
         , HasServer res ctx
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

    hoistServerWithContext _ pc nt s =
            hoistServerWithContext (Proxy @res) pc nt . s

instance HasSwagger (apiType a :> res) =>
         HasSwagger (CDecodeApiArg apiType a :> res) where
    toSwagger _ = toSwagger (Proxy @(apiType a :> res))

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
         , HasServer res ctx
         , ApiHasArg (WithDefaultApiArg apiType) a res
         , Server (apiType a :> res) ~ (Maybe c -> d)
         , Default c
         ) =>
         HasServer (WithDefaultApiArg apiType a :> res) ctx where
    type ServerT (WithDefaultApiArg apiType a :> res) m =
         UnmaybeArg (ServerT (apiType a :> res) m)

    route =
        inRouteServer @(apiType a :> res) route $
        \f a -> f $ fromMaybe def a

    -- The simpliest what we can do here is to delegate to
    -- @hoistServer (Proxy @res)@.
    -- However to perform that we need a knowledge of that
    -- @ServerT (withDefaultApiArg apiType a :> res) m@ is a /function/
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
        \\ inst @(ApiHasArgInvariant (WithDefaultApiArg apiType) a res) @m1
        \\ inst @(ApiHasArgInvariant (WithDefaultApiArg apiType) a res) @m2

instance HasSwagger (apiType a :> res) =>
    HasSwagger (WithDefaultApiArg apiType a :> res) where
    toSwagger _ = toSwagger (Proxy @(apiType a :> res))

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
-- getting acknoledged with api.
data LoggingApi config api

-- | Helper to traverse servant api and apply logging.
data LoggingApiRec config api

type ApiLoggingConfig = LoggerName

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
        updateParamsInfo = do
            let path = const . toText . symbolVal $ Proxy @path
            _ApiParamsLogInfo %~ (path :)

-- | Describes a way to log a single parameter.
class ApiHasArgClass apiType a =>
      ApiCanLogArg apiType a where
    type ApiArgToLog (apiType :: * -> *) a :: *
    type ApiArgToLog apiType a = ApiArg apiType a

    toLogParamInfo
        :: BuildableSafe (ApiArgToLog apiType a)
        => Proxy (apiType a) -> ApiArg apiType a -> SecuredText
    default toLogParamInfo
        :: BuildableSafe (ApiArgToLog apiType a)
        => Proxy (apiType a) -> ApiArgToLog apiType a -> SecuredText
    toLogParamInfo _ param = \sl -> sformat (buildSafe sl) param

instance KnownSymbol s => ApiCanLogArg (Capture s) a

instance ApiCanLogArg (ReqBody ct) a

instance KnownSymbol cs => ApiCanLogArg (QueryParam cs) a where
    type ApiArgToLog (QueryParam cs) a = a
    toLogParamInfo _ mparam =
        \sl -> maybe noEntry (sformat $ buildSafe sl) mparam
      where
        noEntry = colorizeDull White "-"

instance ( ApiHasArgClass apiType a
         , ApiArg apiType a ~ Maybe b
         , ApiCanLogArg apiType a
         ) =>
         ApiCanLogArg (WithDefaultApiArg apiType) a where

instance ( ApiCanLogArg apiType a ) =>
         ApiCanLogArg (CDecodeApiArg apiType) a where

instance ( HasServer (apiType a :> res) ctx
         , HasServer (apiType a :> LoggingApiRec config res) ctx
         , ApiHasArg apiType a res
         , ApiHasArg apiType a (LoggingApiRec config res)
         , ApiCanLogArg apiType a
         , BuildableSafe (ApiArgToLog apiType a)
         ) =>
         HasLoggingServer config (apiType a :> res) ctx where
    routeWithLog =
        inRouteServer @(apiType a :> LoggingApiRec config res) route $
        \(paramsInfo, f) a -> (a `updateParamsInfo` paramsInfo, f a)
      where
        updateParamsInfo a = do
            let paramVal = toLogParamInfo (Proxy @(apiType a)) a
                paramName = apiArgName $ Proxy @(apiType a)
                paramInfo =
                    \sl -> sformat (string%": "%stext) paramName (paramVal sl)
            _ApiParamsLogInfo %~ (paramInfo :)

instance HasLoggingServer config res ctx =>
         HasLoggingServer config (Summary s :> res) ctx where
    routeWithLog = inRouteServer @(Summary s :> LoggingApiRec config res) route identity

instance HasLoggingServer config res ctx =>
         HasLoggingServer config (Description d :> res) ctx where
    routeWithLog = inRouteServer @(Description d :> LoggingApiRec config res) route identity

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
    catchErrors timer $ do
        res <- action
        reportResponse timer res
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
        let loggerName = reflect configP
        usingLoggerName loggerName logAction
    eParamLogs :: Either Text SecuredText
    eParamLogs = case paramsInfo of
        ApiParamsLogInfo info -> Right $ \sl ->
            let params =
                  mconcat $ reverse info <&> \securedParamsInfo ->
                      sformat ("    "%stext%" "%stext%"\n")
                          (colorizeDull White ":>")
                          (securedParamsInfo sl)
            in  sformat ("\n"%stext%"\n"%build) cmethod params
        ApiNoParamsLogInfo why -> Left why
    logWithParamInfo :: MonadIO m => SecuredText -> m ()
    logWithParamInfo securedText =
        case eParamLogs of
            Left e ->
                inLogCtx $ logInfoSP $ \sl ->
                    sformat ("\n"%stext%secretOnlyF sl (" "%stext))
                        (colorizeDull Red "Unexecuted request due to error") e
            Right paramLogs -> do
                inLogCtx $ logInfoSP $ \sl ->
                    sformat (build%" "%build) (paramLogs sl) (securedText sl)
    reportResponse timer resp = do
        durationText <- timer
        logWithParamInfo $ \sl ->
            sformat ("  "%stext%" "%stext%" "%stext
                    %plainOrSecureF sl (stext%stext) (fconst ""%fconst ""))
                (colorizeDull White "Status:")
                (colorizeDull Green "OK")
                durationText
                (colorizeDull White " > ")
                (showResponse resp)
    catchErrors st =
        flip catchError (servantErrHandler st) .
        handleAny (exceptionsHandler st)
    servantErrHandler timer err@ServantErr{..} = do
        durationText <- timer
        let errMsg = sformat (build%" "%string) errHTTPCode errReasonPhrase
        logWithParamInfo $ \_sl ->
            sformat ("  "%stext%" "%stext%" "%stext)
                (colorizeDull White "Status: ")
                (colorizeDull Red errMsg)
                durationText
        throwError err
    exceptionsHandler timer e = do
        durationText <- timer
        logWithParamInfo $ \_sl ->
            sformat ("  "%stext%" "%shown%" "%stext)
                (colorizeDull Red "Error")
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

type CQueryParam s a = CDecodeApiArg (QueryParam s) a
type CCapture s a    = CDecodeApiArg (Capture s) a
type CReqBody c a    = CDecodeApiArg (ReqBody c) a

type DReqBody c a    = WithDefaultApiArg (ReqBody c) a

type DCQueryParam s a = WithDefaultApiArg (CDecodeApiArg $ QueryParam s) a

type DQueryParam s a = WithDefaultApiArg (QueryParam s) a
