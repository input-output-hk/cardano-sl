{-# LANGUAGE NamedFieldPuns #-}
module Test.Integration.Scenario.Node (spec) where

import           Universum

import           Network.HTTP.Types (Status (..))
import           Servant.Client (GenResponse (..), ServantError (..))
import           Test.Hspec (expectationFailure)

import           Test.Integration.Framework.DSL

import qualified Cardano.Wallet.Client.Http as Client
import           Pos.Node.API (ForceNtpCheck (..))

spec :: Scenarios Context
spec = do
    scenario "getNodeSettings" $ do
        void $ successfulRequest Client.getNodeSettings
    scenario "getNodeInfo ForceNtpCheck" $ do
        void $ successfulRequest $ Client.getNodeInfo $- ForceNtpCheck
    scenario "getNodeInfo NoNtpCheck" $ do
        void $ successfulRequest $ Client.getNodeInfo $- NoNtpCheck
    scenario "nextUpdate" $ do
        eresponse <- request Client.nextUpdate
        case eresponse of
            Left (Client.ClientHttpError (FailureResponse (Response { responseStatusCode = Status { statusCode } })))
                | statusCode == 404 -> pure ()
            Right _ ->
                pure ()
            Left err ->
                liftIO
                    . expectationFailure
                    $ "Expected either a 404 response or a success response, got: "
                    <> show err
