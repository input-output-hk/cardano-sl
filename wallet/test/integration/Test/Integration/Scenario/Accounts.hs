module Test.Integration.Scenario.Accounts
    ( spec
    ) where

import           Universum

import qualified Cardano.Wallet.Client.Http as Client
import           Test.Integration.Framework.DSL


spec :: Scenarios Context
spec = do
    scenario "only account's balance can be retrieved (empty balance)" $ do
        fixture <- setup defaultSetup

        response <- request $ Client.getAccountBalance
            $- (fixture ^. wallet . walletId)
            $- defaultAccountId

        verify response
            [ expectFieldEqual amount 0
            ]

    scenario "only account's balance can be retrieved (non empty balance)" $ do
        fixture <- setup $ defaultSetup
            & initialCoins .~ [14, 42]

        response <- request $ Client.getAccountBalance
            $- (fixture ^. wallet . walletId)
            $- defaultAccountId

        verify response
            [ expectFieldEqual amount 56
            ]

    scenario "only account's addresses can be retrieved" $ do
        fixture <- setup defaultSetup

        response <- request $ Client.getAccountAddresses
            $- (fixture ^. wallet . walletId)
            $- defaultAccountId
            $- defaultPage
            $- defaultPerPage
            $- NoFilters

        verify response
            [ expectSuccess
            ]
