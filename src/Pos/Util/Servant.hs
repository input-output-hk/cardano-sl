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
    , CDecodeApi

    , OriginType
    , FromCType (..)
    , ToCType (..)

    , CQueryParam
    , CCapture
    ) where

import           Control.Monad.Except (ExceptT (..))
import           Servant.API          ((:>), Capture, QueryParam, Verb)
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

class ReportDecodeError api where
    -- | Propagate error to servant handler.
    reportDecodeError :: Proxy api -> Text -> Server api

instance ( ReportDecodeError res
         , Server (argType a :> res) ~ (a -> Server res)
         ) =>
         ReportDecodeError (argType a :> res) where
    reportDecodeError _ err = \_ -> reportDecodeError (Proxy @res) err

-- | Wrapper over API argument which says to decode argument with @decodeCType@.
data CDecodeApi argType

type family ApiArg (apiType :: * -> *) a :: *
type instance ApiArg (Capture s) a = a
type instance ApiArg (QueryParam s) a = Maybe a

instance ( HasServer (apiType a :> res) ctx
         , FromCType (ApiArg apiType a)
         , Server (apiType a :> res) ~ (ApiArg apiType a -> Server res)
         , ReportDecodeError res
         ) =>
         HasServer (CDecodeApi (apiType a) :> res) ctx where
    type ServerT (CDecodeApi (apiType a) :> res) m =
         OriginType (ApiArg apiType a) -> ServerT res m
    route _ ctx del =
        route serverProxy ctx $
        del <&> \f a ->
            decodeCType a & either (reportDecodeError (Proxy @res)) f
      where
        serverProxy = Proxy @(apiType a :> res)

-- | For given @CType@ - relative original type
-- (e.g. @CAccountAddress@ - @AccountAddress@)
type family OriginType c :: *

-- | Way to decode from @CType@.
class FromCType c where
    decodeCType :: c -> Either Text (OriginType c)

-- | Way to encode to @CType@.
class ToCType c where
    encodeCType :: OriginType c -> c

-------------------------------------------------------------------------
-- API construction Helpers
-------------------------------------------------------------------------

type CQueryParam s a = CDecodeApi $ QueryParam s a
type CCapture s a    = CDecodeApi $ Capture s a
