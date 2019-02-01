module Test.Integration.Scenario.Transactions
    ( spec
    ) where

import           Universum

import qualified Cardano.Wallet.Client.Http as Client
import           Test.Integration.Framework.DSL


spec :: Scenarios Context
spec = do
    scenario "successful payment appears in the history" $ do
        fixture <- setup $ defaultSetup
            & initialCoins .~ [1000000]

        response <- request $ Client.postTransaction $- Payment
            (defaultSource fixture)
            (defaultDistribution 14 fixture)
            defaultGroupingPolicy
            noSpendingPassword

        verify response
            [ expectTxInHistoryOf (fixture ^. wallet)
            , expectTxStatusEventually [Creating, Applying, InNewestBlocks, Persisted]
            ]

    scenario "estimate fees of well-formed transaction returns" $ do
        fixture <- setup $ defaultSetup
            & initialCoins .~ [1000000]

        response <- request $ Client.getTransactionFee $- Payment
            (defaultSource fixture)
            (defaultDistribution 14 fixture)
            defaultGroupingPolicy
            noSpendingPassword

        verify response
            [ expectSuccess
            ]

    scenario "payment fails when wallet has no funds" $ do
        fixture <- setup $ defaultSetup

        response <- request $ Client.postTransaction $- Payment
            (defaultSource fixture)
            (defaultDistribution 14 fixture)
            defaultGroupingPolicy
            noSpendingPassword

        verify response
            [ expectWalletError (NotEnoughMoney (ErrAvailableBalanceIsInsufficient 0))
            ]

    scenario "payment fails when wallet has insufficient funds" $ do
        fixture <- setup $ defaultSetup
            & initialCoins .~ [14]

        response <- request $ Client.postTransaction $- Payment
            (defaultSource fixture)
            (defaultDistribution 42 fixture)
            defaultGroupingPolicy
            noSpendingPassword

        verify response
            [ expectWalletError (NotEnoughMoney (ErrAvailableBalanceIsInsufficient 14))
            ]

    scenario "payment fails when wallet cannot cover for fee" $ do
        fixture <- setup $ defaultSetup
            & initialCoins .~ [42]

        response <- request $ Client.postTransaction $- Payment
            (defaultSource fixture)
            (defaultDistribution 42 fixture)
            defaultGroupingPolicy
            noSpendingPassword

        verify response
            [ expectWalletError (NotEnoughMoney ErrCannotCoverFee)
            ]

    scenario "balance is increased when valid key is redemeed" $ do
        fixture <- setup defaultSetup

        sequence_
            [ do -- // 1 Redeem a certificate
                response <- request $ Client.redeemAda $- Redemption
                    (ShieldedRedemptionCode "QBYOctbb6fJT/dBDLwg4je+SAvEzEhRxA7wpLdEFhnY=")
                    noRedemptionMnemonic
                    defaultSpendingPassword
                    (fixture ^. wallet . walletId)
                    defaultAccountId

                verify response
                    [ expectTxStatusEventually [InNewestBlocks, Persisted]
                    ]

            , do -- // 2 Control The Balance
                response <- request $ Client.getAccountBalance
                    $- (fixture ^. wallet . walletId)
                    $- defaultAccountId

                verify response
                    [ expectFieldEqual amount 100000
                    ]

            , do -- // 3 Try Redeeming again
                response <- request $ Client.redeemAda $- Redemption
                    (ShieldedRedemptionCode "QBYOctbb6fJT/dBDLwg4je+SAvEzEhRxA7wpLdEFhnY=")
                    noRedemptionMnemonic
                    defaultSpendingPassword
                    (fixture ^. wallet . walletId)
                    defaultAccountId

                verify response
                    [ expectWalletError TxRedemptionDepleted
                    ]
            ]

    scenario "Reg#141: metadata become inconsistent after a wallet is deleted" $ do
        _ <- setup $ defaultSetup
            & initialCoins .~ [14]

        response <- request $ Client.getTransactionIndexFilterSorts
            $- Nothing
            $- Nothing
            $- Nothing
            $- defaultPage
            $- defaultPerPage
            $- NoFilters
            $- NoSorts

        verify response
            [ expectSuccess
            ]

    scenario "Reg#141: metadata become inconsistent after an account is deleted" $ do
        fixture <- setup $ defaultSetup
            & initialCoins .~ [14]

        request_ $ Client.postAccount
            $- fixture ^. wallet . walletId
            $- NewAccount
                noSpendingPassword
                "My Second Account"

        request_ $ Client.deleteAccount
            $- fixture ^. wallet . walletId
            $- defaultAccountId

        response <- request $ Client.getTransactionIndexFilterSorts
            $- Just (fixture ^. wallet . walletId)
            $- Nothing
            $- Nothing
            $- defaultPage
            $- defaultPerPage
            $- NoFilters
            $- NoSorts

        verify response
            [ expectSuccess
            ]

    scenario "Reg#141: metadata becomes inconsistent after a wallet is deleted and restored" $ do
        fixture <- setup $ defaultSetup
            & initialCoins .~ [500000]

        -- // 1 Make sure there's a transaction in the history
        transaction <- request $ Client.postTransaction $- Payment
            (defaultSource fixture)
            (defaultDistribution 42 fixture)
            defaultGroupingPolicy
            noSpendingPassword
        verify transaction
            [ expectTxStatusEventually [InNewestBlocks, Persisted]
            ]

        -- // 2 Remove and then, restore the wallet
        request_ $ Client.deleteWallet $- fixture ^. wallet . walletId
        restoration <- request $ Client.postWallet $- NewWallet
            (fixture ^. backupPhrase)
            noSpendingPassword
            defaultAssuranceLevel
            defaultWalletName
            RestoreWallet
        verify restoration
            [ expectWalletEventuallyRestored
            ]

        -- // 3 Later, check that we can recover that transaction
        verify transaction
            [ expectTxInHistoryOf  (fixture ^. wallet)
            ]


    -- NOTE:
    -- Cases where we have to increase the number of change outputs are hard
    -- to test in practice. We either need:
    --
    -- - A BIG change to cause an overflow (but even with all the genesis
    --   wallets, we don't have enough funds)
    --
    -- - A selection that will have no change such that a new one will be
    --   created for the change. However, the coin selection tends to always
    --   generate a change output.

    -- Initial Selection:      Final Selection:
    --   inputs : [200000]       inputs : [200000]
    --   outputs: [1]            outputs: [1]
    --   changes: [199999]       changes: [28094]
    --   fee+   : 171905         fee+   : 171817
    --
    --           Actual fee: 171905 (+88)
    coinSelectionScenario "no extra inputs, no extra change" [200000] 1

    -- Initial Selection:      Final Selection:
    --   inputs : [171906]       inputs : [171906]
    --   outputs: [1]            outputs: [1]
    --   changes: [171905]       changes: []
    --   fee+   : 171905         fee+   : 167862
    --
    --           Actual fee: 167862 (+4043)
    coinSelectionScenario "empties a wallet" [171906] 1

    -- Initial Selection:      Final Selection:
    --   inputs : [100000]       inputs : [100000, 100000]
    --   outputs: [1]            outputs: [1]
    --   changes: [99999]        changes: [19964]
    --   fee+   : 171905         fee+   : 179947
    --
    --           Actual fee: 180035 (+88)
    coinSelectionScenario "needs one extra input" [100000, 100000] 1

    -- Initial Selection:      Final Selection:
    --   inputs : [30000]        inputs : [30000, 30000, 30000, 30000,
    --                                     30000, 30000, 30000, 30000]
    --   outputs: [42]           outputs: [42]
    --   changes: [29958]        changes: [11055]
    --   fee+   : 171905         fee+   : 228815
    --
    --           Actual fee: 228903 (+88)
    coinSelectionScenario "needs many extra inputs" (replicate 8 30000) 42
  where
    coinSelectionScenario :: String -> [Word64] -> Word64 -> Scenarios Context
    coinSelectionScenario title coins amt =
        scenario ("coin selection: " <> title) $ do
            fixture <- setup $ defaultSetup
                & initialCoins .~ coins

            response <- request $ Client.postTransaction $- Payment
                (defaultSource fixture)
                (defaultDistribution amt fixture)
                defaultGroupingPolicy
                noSpendingPassword

            verify response
                [ expectTxStatusEventually [InNewestBlocks, Persisted]
                ]
