module WaitSpec (spec) where

import Universum
import Test.Hspec
import Servant.Client (BaseUrl (..), Scheme (Https))

import Pos.Util.Servant (single)

import Cardano.Wallet.API.V1.Types (WalletError(..))
import Cardano.Wallet.Client.Easy
import Cardano.Wallet.Client.Wait
import Cardano.Wallet.ProcessUtil (interruptSelf)


spec :: Spec
spec = before mkTestWalletClient $ do
  describe "The waitForSomething procedure" $ do
    it "times out" $ \wc -> do
      res <- waitForSomething exampleRequest (exampleAction True) quickWaitOptions wc
      syncResultError res `shouldBe` Just (SyncErrorTimedOut 0.5)

    it "ignores connection errors" $ \wc -> do
      res <- waitForSomething exampleError (exampleAction False) quickWaitOptions wc
      syncResultError res `shouldBe` Just (SyncErrorTimedOut 0.5)

    it "handles non-existent process -- no request error" $ \wc -> do
      res <- waitForSomething exampleRequest (exampleAction False) waitOptionsNoPID wc
      syncResultError res `shouldBe` Just (SyncErrorProcessDied 99999)

    it "handles non-existent process -- request error" $ \wc -> do
      res <- waitForSomething exampleError (exampleAction True) waitOptionsNoPID wc
      syncResultError res `shouldBe` Just (SyncErrorProcessDied 99999)

    it "handles exception" $ \wc ->  do
      res <- waitForSomething exampleUnhandledError (exampleAction False) quickWaitOptions wc
      syncResultError res `shouldSatisfy` isSyncErrorException

    it "returns a result on success" $ \wc -> do
      res <- waitForSomething exampleRequest (exampleAction False) quickWaitOptions wc
      syncResultError res `shouldSatisfy` isNothing
      map snd (syncResultData res) `shouldBe` [5]

    it "handles SIGINT" $ \wc -> do
      req <- interruptSelfRequest
      res <- waitForSomething req (exampleAction True) quickWaitOptions wc
      syncResultError res `shouldBe` Just SyncErrorInterrupted

-- | Wait options which time out after 500ms
quickWaitOptions :: WaitOptions
quickWaitOptions = WaitOptions
  { waitTimeoutSeconds = Just 0.5
  , waitProcessID = Nothing
  , waitIntervalSeconds = 0.1
  }

-- | Wait options, with an impossible PID
waitOptionsNoPID :: WaitOptions
waitOptionsNoPID = quickWaitOptions { waitProcessID = Just 99999 }

mkTestWalletClient :: IO (WalletClient IO)
mkTestWalletClient = walletClientFromConfig cfg
  where cfg = ConnectConfig { cfgClientAuth = Nothing
                            , cfgCACertFile = Nothing
                            , cfgAuthenticateServer = AllowInsecure
                            , cfgBaseUrl = BaseUrl Https "localhost" 9999 ""
                            }

newtype TestRes = TestRes { unTestRes :: Int } deriving (Show, Eq)

exampleRequest :: WalletClient IO -> Resp IO TestRes
exampleRequest = const (pure (Right (single (TestRes 4))))

exampleUnhandledError :: WalletClient IO -> Resp IO TestRes
exampleUnhandledError = const (pure (Left (UnknownClientError (error "hello"))))

-- | Returns a request function which interrupts the process on the
-- third time that it's run.
interruptSelfRequest :: IO (WalletClient IO -> Resp IO TestRes)
interruptSelfRequest = do
  c <- newIORef (0 :: Int)
  pure $ \wc -> do
    v <- readIORef c
    when (v >= 2) interruptSelf
    writeIORef c (v + 1)
    exampleRequest wc

exampleError :: WalletClient IO -> Resp IO TestRes
exampleError = const (pure (Left (ClientWalletError WalletNotFound)))

exampleAction :: Bool -> TestRes -> IO (Bool, Text, Int)
exampleAction finished = const (pure (finished, "test", 5))

isSyncErrorException :: Maybe SyncError -> Bool
isSyncErrorException (Just (SyncErrorException _)) = True
isSyncErrorException _ = False
