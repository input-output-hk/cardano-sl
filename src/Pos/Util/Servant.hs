{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Some utilites for more flexible servant usage.

module Pos.Util.Servant
    ( ModifiesApiRes (..)
    , VerbMod

    , ReportDecodeError (..)
    , CDecodeArg

    , FromCType (..)
    , ToCType (..)

    , CQueryParam
    , CCapture
    , CReqBody
    ) where

import           Control.Monad.Except (ExceptT (..))
import           Formatting           (formatToString, sformat, shown, stext, string, (%))
import           Servant.API          ((:>), Capture, QueryParam, ReqBody, Verb)
import           Servant.Server       (Handler (..), HasServer (..), ServantErr, Server)
import           Universum


-------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------

mapHandler :: (IO (Either ServantErr a) -> IO (Either ServantErr b))
           -> Handler a
           -> Handler b
mapHandler f = Handler . ExceptT . f . runExceptT . runHandler'

-------------------------------------------------------------------------
-- General useful families
-------------------------------------------------------------------------

-- | Proves info about argument specifier of servant API.
class ApiHasArg apiType where
    -- | For arguments-specifiers of API, get argument type.
    type ApiArg (apiType :: * -> *) a :: *
    type ApiArg apiType a = a

    -- | Name of argument.
    -- E.g. name of argument specified by @Capture "nyan"@ is /nyan/.
    apiArgName :: Proxy apiType -> String
    default apiArgName
        :: forall n someApiType. (KnownSymbol n, someApiType n ~ apiType)
        => Proxy (someApiType n) -> String
    apiArgName _ = formatToString (shown%" field") $ symbolVal (Proxy @n)

instance KnownSymbol s => ApiHasArg (Capture s) where

instance KnownSymbol s => ApiHasArg (QueryParam s) where
    type ApiArg (QueryParam s) a = Maybe a

instance ApiHasArg (ReqBody ct) where
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

    route _ ctx del = route verbProxy ctx (handlerCatch <$> del)
      where
        verbProxy = Proxy @(Verb mt st ct $ ApiModifiedRes mod a)
        handlerCatch = mapHandler $ modifyApiResult (Proxy @mod)

-------------------------------------------------------------------------
-- Mapping API arguments
-------------------------------------------------------------------------

-- | For many types with nice structure there exist a @CType@ - intermediate
-- form (more string-like in some sense) which has automatically derived
-- JSON instances and is used by daedalus-bridge.
class FromCType c where
   -- | For given @CType@ - relative original type
   -- (e.g. @CAccountAddress@ - @AccountAddress@).
   type family FromOriginType c :: *
   type instance FromOriginType c = ToOriginType c

   -- | Way to decode from @CType@.
   decodeCType :: c -> Either Text (FromOriginType c)

-- This is not used for now
class ToCType c where
   -- | Same as 'FromOriginType'. For some types they actually differ,
   -- e.g. 'PassPhrase' is decoded from @Maybe CPassPhrase@ but encoded
   -- into just @PassPhrase@.
   type family ToOriginType c :: *
   type instance ToOriginType c = FromOriginType c

   -- | Way to encode to @CType@.
   encodeCType :: ToOriginType c -> c

-- | Allows to throw error from any part of innards of servant API.
class ReportDecodeError api where
    -- | Propagate error to servant handler.
    reportDecodeError :: Proxy api -> Text -> Server api

instance ( ReportDecodeError res
         , Server (argType a :> res) ~ (ApiArg argType a -> Server res)
         ) =>
         ReportDecodeError (argType a :> res) where
    reportDecodeError _ err = \_ -> reportDecodeError (Proxy @res) err


-- | Wrapper over API argument specifier which says to decode specified argument
-- with 'decodeCType'.
data CDecodeArg (argType :: * -> *) a

instance ApiHasArg apiType => ApiHasArg (CDecodeArg apiType) where
    type ApiArg (CDecodeArg apiType) a = FromOriginType (ApiArg apiType a)
    apiArgName _ = apiArgName (Proxy @apiType)

instance ( HasServer (apiType a :> res) ctx
         , ApiHasArg apiType
         , Server (apiType a :> res) ~ (ApiArg apiType a -> Server res)
         , FromCType (ApiArg apiType a)
         , ReportDecodeError res
         ) =>
         HasServer (CDecodeArg apiType a :> res) ctx where
    type ServerT (CDecodeArg apiType a :> res) m =
         FromOriginType (ApiArg apiType a) -> ServerT res m
    route _ ctx del =
        route (Proxy @(apiType a :> res)) ctx $
        del <&> \f a -> decodeCType a & either reportError f
      where
        reportError err =
            reportDecodeError (Proxy @res) $
            sformat ("(in "%string%") "%stext)
                (apiArgName $ Proxy @apiType)
                err

-------------------------------------------------------------------------
-- API construction Helpers
-------------------------------------------------------------------------

type CQueryParam s a = CDecodeArg (QueryParam s) a
type CCapture s a    = CDecodeArg (Capture s) a
type CReqBody c a    = CDecodeArg (ReqBody c) a
