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
    ) where

import           Control.Monad.Except (ExceptT (..))
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

class ToCType c where
   -- | Same as 'FromOriginType'. For some types they actually differ,
   -- e.g. 'CPassPhrase'.
   type family ToOriginType c :: *
   type instance ToOriginType c = FromOriginType c

   -- | Way to encode to @CType@.
   encodeCType :: ToOriginType c -> c


-- | For arguments-specifiers of API, get argument type.
type family ApiArg (apiType :: * -> *) a :: *
type instance ApiArg (Capture s) a = a
type instance ApiArg (QueryParam s) a = Maybe a
type instance ApiArg (ReqBody ct) a = a


class ReportDecodeError api where
    -- | Propagate error to servant handler.
    reportDecodeError :: Proxy api -> Text -> Server api

instance ( ReportDecodeError res
         , Server (argType a :> res) ~ (ApiArg argType a -> Server res)
         ) =>
         ReportDecodeError (argType a :> res) where
    reportDecodeError _ err = \_ -> reportDecodeError (Proxy @res) err


-- | Wrapper over API argument which says to decode specified argument with
-- 'decodeCType'.
data CDecodeArg (argType :: * -> *) a

type instance ApiArg (CDecodeArg apiType) a = FromOriginType (ApiArg apiType a)

instance ( HasServer (apiType a :> res) ctx
         , FromCType (ApiArg apiType a)
         , Server (apiType a :> res) ~ (ApiArg apiType a -> Server res)
         , ReportDecodeError res
         ) =>
         HasServer (CDecodeArg apiType a :> res) ctx where
    type ServerT (CDecodeArg apiType a :> res) m =
         FromOriginType (ApiArg apiType a) -> ServerT res m
    route _ ctx del =
        route serverProxy ctx $
        del <&> \f a ->
            decodeCType a & either (reportDecodeError (Proxy @res)) f
      where
        serverProxy = Proxy @(apiType a :> res)

-------------------------------------------------------------------------
-- API construction Helpers
-------------------------------------------------------------------------

type CQueryParam s a = CDecodeArg (QueryParam s) a
type CCapture s a    = CDecodeArg (Capture s) a
