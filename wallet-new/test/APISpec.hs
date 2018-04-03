{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module APISpec where

import qualified Prelude
import           Universum

import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as BS
import           Data.Default (def)
import qualified Data.Text.Encoding as Text
import           Network.HTTP.Client hiding (Proxy)
import           Network.HTTP.Types
import           Ntp.Client (withoutNtpClient)
import qualified Pos.Diffusion.Types as D
import           Pos.Util.CompileInfo (withCompileInfo)
import           Pos.Wallet.WalletMode (WalletMempoolExt)
import           Pos.Wallet.Web.Mode (WalletWebModeContext (..))
import           Pos.Wallet.Web.Sockets (ConnectionsVar)
import           Pos.Wallet.Web.State (WalletDB)
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)
import           Pos.WorkMode (RealModeContext (..))
import           Serokell.AcidState.ExtendedState (openMemoryExtendedState)

import           Servant
import           Servant.QuickCheck
import           Servant.QuickCheck.Internal
import           System.Directory
import           Test.Hspec
import           Test.Pos.Configuration (withDefConfigurations)
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Types
import qualified Cardano.Wallet.API.V1 as V0
import qualified Cardano.Wallet.API.V1 as V1
import qualified Cardano.Wallet.API.V1.LegacyHandlers as V0
import qualified Cardano.Wallet.API.V1.LegacyHandlers as V1
import qualified Cardano.Wallet.API.V1.Migration as Migration
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Types ()

--
-- Instances to allow use of `servant-quickcheck`.
--

instance HasGenRequest (apiType a :> sub) =>
         HasGenRequest (WithDefaultApiArg apiType a :> sub) where
    genRequest _ = genRequest (Proxy @(apiType a :> sub))

instance HasGenRequest (argA a :> argB a :> sub) =>
         HasGenRequest (AlternativeApiArg argA argB a :> sub) where
    genRequest _ = genRequest (Proxy @(argA a :> argB a :> sub))

-- NOTE(adinapoli): This can be improved to produce proper filtering & sorting
-- queries.
instance HasGenRequest sub => HasGenRequest (SortBy syms res :> sub) where
    genRequest _ = genRequest (Proxy @sub)

instance HasGenRequest sub => HasGenRequest (FilterBy syms res :> sub) where
    genRequest _ = genRequest (Proxy @sub)

instance HasGenRequest sub => HasGenRequest (Tags tags :> sub) where
    genRequest _ = genRequest (Proxy :: Proxy sub)

instance HasGenRequest (sub :: *) => HasGenRequest (WalletRequestParams :> sub) where
    genRequest _ = genRequest (Proxy @(WithWalletRequestParams sub))

--
-- RESTful-abiding predicates
--

-- | Checks that every DELETE request should return a 204 NoContent.
deleteReqShouldReturn204 :: RequestPredicate
deleteReqShouldReturn204 = RequestPredicate $ \req mgr ->
     if (method req == methodDelete)
       then do
         resp <- httpLbs req mgr
         let status = responseStatus resp
         when (statusIsSuccessful status && status /= status204) $
           throwM $ PredicateFailure "deleteReqShouldReturn204" (Just req) resp
         return [resp]
       else return []

-- | Checks that every PUT request is idempotent. Calling an endpoint with a PUT
-- twice should return the same result.
putIdempotency :: RequestPredicate
putIdempotency = RequestPredicate $ \req mgr ->
     if (method req == methodPut)
       then do
         resp1 <- httpLbs req mgr
         resp2 <- httpLbs req mgr
         let body1 = responseBody resp1
         let body2 = responseBody resp2
         when (body1 /= body2) $
           throwM $ PredicateFailure "putIdempotency" (Just req) resp1
         return [resp1, resp2]
       else return []

-- | Checks that every request which is not a 204 No Content
-- does not have an empty body, but it always returns something.
noEmptyBody :: RequestPredicate
noEmptyBody = RequestPredicate $ \req mgr -> do
  resp <- httpLbs req mgr
  let body   = responseBody resp
  let status = responseStatus resp
  when (status /= status204 && body == mempty) $
    throwM $ PredicateFailure "noEmptyBody" (Just req) resp
  return [resp]

-- | All the predicates we want to enforce in our API.
predicates :: Predicates
predicates = not500
         <%> deleteReqShouldReturn204
         <%> putIdempotency
         <%> noEmptyBody
         <%> mempty

-- | "Lowers" V0 Handlers from our domain-specific monad to a @Servant@ 'Handler'.
v0Server :: ( Migration.HasConfigurations
            , Migration.HasCompileInfo
            ) => D.Diffusion Migration.MonadV1 -> IO (Server V0.API)
v0Server diffusion = do
  -- TODO(adinapoli): If the monadic stack ends up diverging between V0 and V1,
  -- it's obviously incorrect using 'testV1Context' here.
  ctx <- testV1Context
  withoutNtpClient $ \ntpStatus ->
    return (V0.handlers (Migration.v1MonadNat ctx) diffusion ntpStatus)

-- | "Lowers" V1 Handlers from our domain-specific monad to a @Servant@ 'Handler'.
v1Server :: ( Migration.HasConfigurations
            , Migration.HasCompileInfo
            ) => D.Diffusion Migration.MonadV1 -> IO (Server V1.API)
v1Server diffusion = do
  ctx <- testV1Context
  withoutNtpClient $ \ntpStatus ->
    return (V1.handlers (Migration.v1MonadNat ctx) diffusion ntpStatus)

-- | Returns a test 'V1Context' which can be used for the API specs.
-- Such context will use an in-memory database.
testV1Context :: Migration.HasConfiguration => IO Migration.V1Context
testV1Context =
    WalletWebModeContext <$> testStorage
                         <*> testConnectionsVar
                         <*> testSyncQueue
                         <*> testRealModeContext
  where
    testStorage :: IO WalletDB
    testStorage = openMemoryExtendedState def

    testConnectionsVar :: IO ConnectionsVar
    testConnectionsVar = STM.newTVarIO def

    testSyncQueue :: IO SyncQueue
    testSyncQueue = STM.newTQueueIO

    -- For some categories of tests we won't hit the 'RealModeContext', so that's safe
    -- for now to leave it unimplemented.
    testRealModeContext :: IO (RealModeContext WalletMempoolExt)
    testRealModeContext = return (error "testRealModeContext is currently unimplemented")

serverLayout :: ByteString
serverLayout = Text.encodeUtf8 (layout (Proxy @V1.API))

-- Our API apparently is returning JSON Arrays which is considered bad practice as very old
-- browsers can be hacked: https://haacked.com/archive/2009/06/25/json-hijacking.aspx/
-- The general consensus, after discussing this with the team, is that we can be moderately safe.
spec :: Spec
spec = withCompileInfo def $ do
    withDefConfigurations $ \_ -> do
        xdescribe "Servant API Properties" $ do
            it "V0 API follows best practices & is RESTful abiding" $ do
                ddl <- D.dummyDiffusionLayer
                withServantServer (Proxy @V0.API) (v0Server (D.diffusion ddl)) $ \burl ->
                    serverSatisfies (Proxy @V0.API) burl stdArgs predicates
            it "V1 API follows best practices & is RESTful abiding" $ do
                ddl <- D.dummyDiffusionLayer
                withServantServer (Proxy @V1.API) (v1Server (D.diffusion ddl)) $ \burl ->
                    serverSatisfies (Proxy @V1.API) burl stdArgs predicates

    describe "Servant Layout" $ around_ withTestDirectory $ do
        let layoutPath = "./test/golden/api-layout.txt"
            newLayoutPath = layoutPath <> ".new"
        it "has not changed" $ do
            oldLayout <- BS.readFile layoutPath `catch` \(_err :: SomeException) -> pure ""
            when (oldLayout /= serverLayout) $ do
                BS.writeFile newLayoutPath serverLayout
                expectationFailure $ Prelude.unlines
                    [ "The API layout has changed!!! The new layout has been written to:"
                    , "    " <> newLayoutPath
                    , "If this was intentional and correct, move the new layout path to:"
                    , "    " <> layoutPath
                    , "Command:"
                    , "    mv " <> newLayoutPath <> " " <> layoutPath
                    ]

-- | This is a hack that sets the CWD to the correct directory to access
-- golden tests. `stack` will run tests at the top level of the git
-- project, while `cabal` and the Nix CI will run tests at the `wallet-new`
-- directory. This function ensures that we are in the `wallet-new`
-- directory for the execution of this test.
withTestDirectory :: IO () -> IO ()
withTestDirectory action = void . runMaybeT $ do
    dir <- lift getCurrentDirectory
    entries <- lift $ listDirectory dir
    guard ("cardano-sl-wallet-new.cabal" `notElem` entries)
    guard ("wallet-new" `elem` entries)
    lift $ do
        bracket_ (setCurrentDirectory =<< makeAbsolute "wallet-new")
                 (setCurrentDirectory dir)
                 action
