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
    , CDecodeApiArg

    , OriginType
    , FromCType (..)
    , ToCType (..)

    , WithDefaultApiArg

    , CQueryParam
    , CCapture
    , CReqBody
    , DCQueryParam
    ) where

import           Control.Monad.Except (ExceptT (..))
import           Data.Default         (Default (..))
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
-- Mapping API arguments: client types decoding
-------------------------------------------------------------------------

-- | For many types with nice structure there exist a /client type/ -
-- intermediate form (more string-like in some sense) which has automatically
-- derived JSON instances and is used by daedalus-bridge.
-- | This family, for given /client type/, gets relative original type
-- (e.g. @CAccountAddress@ - @AccountAddress@).
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
data CDecodeApiArg (argType :: * -> *) a

instance ApiHasArg apiType => ApiHasArg (CDecodeApiArg apiType) where
    type ApiArg (CDecodeApiArg apiType) a = OriginType (ApiArg apiType a)
    apiArgName _ = apiArgName (Proxy @apiType)

instance ( HasServer (apiType a :> res) ctx
         , ApiHasArg apiType
         , Server (apiType a :> res) ~ (ApiArg apiType a -> Server res)
         , FromCType (ApiArg apiType a)
         , ReportDecodeError res
         ) =>
         HasServer (CDecodeApiArg apiType a :> res) ctx where
    type ServerT (CDecodeApiArg apiType a :> res) m =
         OriginType (ApiArg apiType a) -> ServerT res m
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
-- Mapping API arguments: defaults
-------------------------------------------------------------------------

type family UnmaybeArg a where
    UnmaybeArg (Maybe a -> b) = a -> b

type family Unmaybe a where
    Unmaybe (Maybe a) = a

data WithDefaultApiArg (argType :: * -> *) a

-- TODO: why is it needed at all? 'DefaultApiArg' is used only at top
-- layer for given argument
instance ApiHasArg apiType => ApiHasArg (WithDefaultApiArg apiType) where
    type ApiArg (WithDefaultApiArg apiType) a = Unmaybe (ApiArg apiType a)
    apiArgName _ = apiArgName (Proxy @apiType)

instance ( HasServer (apiType a :> res) ctx
         , Server (apiType a :> res) ~ (Maybe c -> d)
         , Default c
         ) =>
         HasServer (WithDefaultApiArg apiType a :> res) ctx where
    type ServerT (WithDefaultApiArg apiType a :> res) m =
         UnmaybeArg (ServerT (apiType a :> res) m)
    route _ ctx del =
        route (Proxy @(apiType a :> res)) ctx $
        del <&> \f a -> f $ fromMaybe def a

-------------------------------------------------------------------------
-- API construction Helpers
-------------------------------------------------------------------------

type CQueryParam s a = CDecodeApiArg (QueryParam s) a
type CCapture s a    = CDecodeApiArg (Capture s) a
type CReqBody c a    = CDecodeApiArg (ReqBody c) a

type DCQueryParam s a = WithDefaultApiArg (CDecodeApiArg $ QueryParam s) a
