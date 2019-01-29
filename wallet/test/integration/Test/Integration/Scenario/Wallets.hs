module Test.Integration.Scenario.Wallets
    ( spec
    ) where

import           Universum

import           Cardano.Wallet.Client.Http (ClientError, Wallet)
import qualified Cardano.Wallet.Client.Http as Client
import           Test.Hspec (describe)
import           Test.Integration.Framework.DSL

spec :: Scenarios Context
spec = do
    scenario "WALLETS_DELETE_01 - deleted wallet is not available" $ do
        fixture <- setup $ defaultSetup
            & walletName .~ "漢ę ó ł ąś ł żźćń字"
            & mnemonicWords .~ testBackupPhrase

        successfulRequest $ Client.deleteWallet
            $- (fixture ^. wallet . walletId)

        response02 <- request $ Client.getWallet
            $- (fixture ^. wallet . walletId)

        verify response02
            [ expectWalletError (WalletNotFound)
            ]

    scenario "WALLETS_LIST_01 - One can list all wallets without providing any parameters" $ do
        fixtures <- forM (zip [1..3] [NormalAssurance, NormalAssurance, StrictAssurance]) $ \(name, level) -> do
            setup $ defaultSetup
                & walletName .~ show (name :: Int)
                & assuranceLevel .~ level
        forM_ fixtures $ \fixture -> do
            response <- request $ Client.getWallet
                $- (fixture ^. wallet . walletId)
            verify response
                [ expectFieldEqual walletName (fixture ^. wallet . walletName)
                , expectFieldEqual assuranceLevel (fixture ^. wallet .  assuranceLevel)
                ]

        getWalletsResp <- request $ Client.getWallets
        verify getWalletsResp
            [ expectSuccess
            , expectListSizeEqual 3
            ]

    describe "WALLETS_LIST_02 - One can set page >= 1 and per_page [1..50] on the results." $ do

        let failingScenario = "1 wallet; page=9223372036854775807 & per_page=50 => 0 wallets returned"

        let matrix =
                [ ( "2 wallets; page=1 & per_page=1 => 1 wallet returned"
                  , 2
                  , Just (Client.Page 1)
                  , Just (Client.PerPage 1)
                  , [ expectSuccess
                    , expectListSizeEqual 1
                    ]
                  )
                , ( "6 wallets; page=3 & per_page=2 => 2 wallets returned"
                  , 6
                  , Just (Client.Page 3)
                  , Just (Client.PerPage 2)
                  , [ expectSuccess
                    , expectListSizeEqual 2
                    ]
                  )
                , ( "4 wallets; per_page=3 => 3 wallets returned"
                  , 4
                  , Nothing
                  , Just (Client.PerPage 3)
                  , [ expectSuccess
                    , expectListSizeEqual 3
                    ]
                  )
                , ( "11 wallets; page=1 => 10 wallets returned"
                  , 11
                  , Just (Client.Page 1)
                  , Nothing
                  , [ expectSuccess
                    , expectListSizeEqual 10
                    ]
                  )
                , ( failingScenario
                  , 1
                  , Just (Client.Page 9223372036854775807)
                  , Just (Client.PerPage 50)
                  , [ expectSuccess
                    , expectListSizeEqual 0
                    ]
                  )
                ]

        forM_ matrix $ \(title, walletsNumber, page, perPage, expectations) -> scenario title $ do

            forM_ ([1..walletsNumber]) $ \name -> do
                when (title == failingScenario) $
                    pendingWith "Test fails due to bug #213"

                setup $ defaultSetup
                    & walletName .~ show (name :: Int)

            response <- request $ Client.getWalletIndexFilterSorts
                $- page
                $- perPage
                $- NoFilters
                $- NoSorts
            verify response expectations

    describe "WALLETS_LIST_02 - One gets error when page and/or per_page have non-supported values" $ do

        let matrix =
                [ ( "api/v1/wallets?page=0"
                  , [ expectError
                    -- add expectClientHttpError message validation
                    ]
                  )
                , ( "api/v1/wallets?page=-1"
                  , [ expectError
                    -- add expectClientHttpError message validation
                    ]
                  )
                , ( "api/v1/wallets?page=0&per_page=35"
                  , [ expectError
                    -- add expectClientHttpError message validation
                    ]
                  )
                , ( "api/v1/wallets?page=1漢patate字"
                  , [ expectError
                    -- add expectClientHttpError message validation
                    ]
                  )
                , ( "api/v1/wallets?page=9223372036854775808"
                  , [ expectError
                    -- add expectClientHttpError message validation
                    ]
                  )
                , ( "api/v1/wallets?page=-9223372036854775809"
                  , [ expectError
                    -- add expectClientHttpError message validation
                    ]
                  )
                , ( "api/v1/wallets?per_page=0"
                  , [ expectError
                    -- add expectClientHttpError message validation
                    ]
                  )
                , ( "api/v1/wallets?per_page=-1&page=1"
                  , [ expectError
                    -- add expectClientHttpError message validation
                    ]
                  )
                , ( "api/v1/wallets?per_page=51"
                  , [ expectError
                    -- add expectClientHttpError message validation
                    ]
                  )
                , ( "api/v1/wallets?per_page=5漢patate字"
                  , [ expectError
                    -- add expectClientHttpError message validation
                    ]
                  )
                ]

        forM_ matrix $ \(endpoint, expectations) -> scenario endpoint $ do
            _ <- setup $ defaultSetup
            resp <- unsafeRequest ("GET", fromString endpoint) $ Nothing
            verify (resp :: Either ClientError [Wallet]) expectations

    describe "WALLETS_LIST_03 - One can filter wallets by balance" $ do

        let matrix =
                [ ( "api/v1/wallets?balance=EQ%5B3%5D"
                  , [ expectSuccess
                    , expectListSizeEqual 1
                    , expectListItemFieldEqual 0 amount 3
                    ]
                  )
                , ( "api/v1/wallets?balance=6"
                  , [ expectSuccess
                    , expectListSizeEqual 2
                    , expectListItemFieldEqual 0 amount 6
                    , expectListItemFieldEqual 1 amount 6
                    ]
                  )
                , ( "api/v1/wallets?balance=LT%5B6%5D&sort_by=balance"
                  , [ expectSuccess
                    , expectListSizeEqual 2
                    , expectListItemFieldEqual 0 amount 3
                    , expectListItemFieldEqual 1 amount 0
                    ]
                  )
                , ( "api/v1/wallets?balance=GT%5B6%5D"
                  , [ expectSuccess
                    , expectListSizeEqual 1
                    , expectListItemFieldEqual 0 amount 9
                    ]
                  )
                , ( "api/v1/wallets?balance=GTE%5B6%5D&sort_by=balance"
                  , [ expectSuccess
                    , expectListSizeEqual 3
                    , expectListItemFieldEqual 0 amount 9
                    , expectListItemFieldEqual 1 amount 6
                    , expectListItemFieldEqual 2 amount 6
                    ]
                  )
                , ( "api/v1/wallets?balance=LTE%5B6%5D&sort_by=balance"
                  , [ expectSuccess
                    , expectListSizeEqual 4
                    , expectListItemFieldEqual 0 amount 6
                    , expectListItemFieldEqual 1 amount 6
                    , expectListItemFieldEqual 2 amount 6
                    , expectListItemFieldEqual 3 amount 0
                    ]
                  )
                , ( "api/v1/wallets?balance=RANGE%5B6%3,6D&sort_by=balance"
                  , [ expectSuccess
                    , expectListSizeEqual 3
                    , expectListItemFieldEqual 0 amount 6
                    , expectListItemFieldEqual 1 amount 6
                    , expectListItemFieldEqual 2 amount 3
                    ]
                  )
                , ( "api/v1/wallets?balance=RANGE%5B6%3,0,9D&sort_by=balance"
                  , [ expectSuccess
                    , expectListSizeEqual 3
                    , expectListItemFieldEqual 0 amount 9
                    , expectListItemFieldEqual 1 amount 3
                    , expectListItemFieldEqual 2 amount 0
                    ]
                  )
                ]

        forM_ matrix $ \(endpoint, expectations) -> scenario endpoint $ do
            pendingWith "Test fails due to bug #220"
            forM_ ([3,6,6,9,0]) $ \coins -> do
                setup $ defaultSetup
                    & initialCoins .~ [coins]

            resp <- unsafeRequest ("GET", fromString endpoint) $ Nothing
            verify (resp :: Either ClientError [Wallet]) expectations

    scenario "WALLETS_LIST_03 - One can filter wallets by wallet id" $ do
        -- EQ[value] : only allow values equal to value

        fixtures <- forM ([1,2]) $ \name -> do
            setup $ defaultSetup
                & walletName .~ show (name :: Int)

        let walletIds =
                [ (fixtures !! 0) ^. wallet . walletId
                , (fixtures !! 1) ^. wallet . walletId
                ]
        let endpoint = "api/v1/wallets?id=" <> ( fromWalletId $ walletIds !! 0 )

        resp <- unsafeRequest ("GET", endpoint) $ Nothing
        verify (resp :: Either ClientError [Wallet])
            [ expectSuccess
            , expectListSizeEqual 1
            , expectListItemFieldEqual 0 walletId ( walletIds !! 0 )
            ]

    scenario "WALLETS_LIST_03 - One can filter wallets by wallet id -- EQ[value]" $ do
        -- EQ[value] : only allow values equal to value

        fixtures <- forM ([1,2]) $ \name -> do
            setup $ defaultSetup
                & walletName .~ show (name :: Int)

        let walletIds =
                [ (fixtures !! 0) ^. wallet . walletId
                , (fixtures !! 1) ^. wallet . walletId
                ]
        let endpoint = "api/v1/wallets?id=EQ%5B" <> ( fromWalletId $ walletIds !! 0 ) <> ("%5D" :: Text)
        resp <- unsafeRequest ("GET", endpoint) $ Nothing
        verify (resp :: Either ClientError [Wallet])
            [ expectSuccess
            , expectListSizeEqual 1
            , expectListItemFieldEqual 0 walletId ( walletIds !! 0 )
            ]

    scenario "WALLETS_LIST_03 - One can filter wallets by wallet id -- LT[value]" $ do
        -- LT[value] : allow resource with attribute less than the value

        fixtures <- forM ([1,2]) $ \name -> do
            setup $ defaultSetup
                & walletName .~ show (name :: Int)

        let sortedWalletIds =
                sort [ fromWalletId $ (fixtures !! 0) ^. wallet . walletId
                     , fromWalletId $ (fixtures !! 1) ^. wallet . walletId
                     ]

        let endpoint = "api/v1/wallets?id=LT%5B" <> ( sortedWalletIds !! 1 ) <> ("%5D" :: Text)
        resp <- unsafeRequest ("GET", endpoint) $ Nothing
        verify (resp :: Either ClientError [Wallet])
            [ expectSuccess
            , expectListSizeEqual 1
            , expectListItemFieldEqual 0 walletId ( Client.WalletId $ sortedWalletIds !! 0 )
            ]

    scenario "WALLETS_LIST_03 - One can filter wallets by wallet id -- GT[value]" $ do
        -- GT[value] : allow objects with an attribute greater than the value

        fixtures <- forM ([1,2]) $ \name -> do
            setup $ defaultSetup
                & walletName .~ show (name :: Int)

        let sortedWalletIds =
                sort [ fromWalletId $ (fixtures !! 0) ^. wallet . walletId
                     , fromWalletId $ (fixtures !! 1) ^. wallet . walletId
                     ]

        let endpoint = "api/v1/wallets?id=GT%5B" <> ( sortedWalletIds !! 0 ) <> ("%5D" :: Text)

        resp <- unsafeRequest ("GET", endpoint) $ Nothing
        verify (resp :: Either ClientError [Wallet])
            [ expectSuccess
            , expectListSizeEqual 1
            , expectListItemFieldEqual 0 walletId ( Client.WalletId $ sortedWalletIds !! 1 )
            ]

    scenario "WALLETS_LIST_03 - One can filter wallets by wallet id -- GTE[value]" $ do
        -- GTE[value] : allow objects with an attribute at least the value

        fixtures <- forM ([1,2]) $ \name -> do
            setup $ defaultSetup
                & walletName .~ show (name :: Int)

        let walletIds =
                     [ fromWalletId $ (fixtures !! 0) ^. wallet . walletId
                     , fromWalletId $ (fixtures !! 1) ^. wallet . walletId
                     ]

        let endpoint = "api/v1/wallets?sort_by=created_at&id=GTE%5B" <> ( sort ( walletIds ) !! 0 ) <> ("%5D" :: Text)
        resp <- unsafeRequest ("GET", endpoint) $ Nothing
        verify (resp :: Either ClientError [Wallet])
            [ expectSuccess
            , expectListSizeEqual 2
            , expectListItemFieldEqual 0 walletId ( Client.WalletId $ walletIds !! 1 )
            , expectListItemFieldEqual 1 walletId ( Client.WalletId $ walletIds !! 0 )
            ]

    scenario "WALLETS_LIST_03 - One can filter wallets by wallet id -- LTE[value]" $ do
        -- LTE[value] : allow objects with an attribute at most the value

        fixtures <- forM ([1,2]) $ \name -> do
            setup $ defaultSetup
                & walletName .~ show (name :: Int)

        let walletIds =
                     [ fromWalletId $ (fixtures !! 0) ^. wallet . walletId
                     , fromWalletId $ (fixtures !! 1) ^. wallet . walletId
                     ]

        let endpoint = "api/v1/wallets?sort_by=created_at&id=LTE%5B" <> ( sort ( walletIds ) !! 1 ) <> ("%5D" :: Text)
        resp <- unsafeRequest ("GET", endpoint) $ Nothing
        verify (resp :: Either ClientError [Wallet])
            [ expectSuccess
            , expectListSizeEqual 2
            , expectListItemFieldEqual 0 walletId ( Client.WalletId $ walletIds !! 1 )
            , expectListItemFieldEqual 1 walletId ( Client.WalletId $ walletIds !! 0 )
            ]

    scenario "WALLETS_LIST_03 - One can filter wallets by wallet id -- RANGE[value]" $ do
        -- RANGE[lo,hi] : allow objects with the attribute in the range between lo and hi

        fixtures <- forM ([1,2,3]) $ \name -> do
            setup $ defaultSetup
                & walletName .~ show (name :: Int)

        let walletIds =
                     [ fromWalletId $ (fixtures !! 0) ^. wallet . walletId
                     , fromWalletId $ (fixtures !! 1) ^. wallet . walletId
                     , fromWalletId $ (fixtures !! 2) ^. wallet . walletId
                     ]
        let sortedWalletIds = sort walletIds

        let endpoint = "api/v1/wallets?sort_by=created_at&id=RANGE%5B" <> ( sortedWalletIds !! 0 )
                        <> "," <> ( sortedWalletIds !! 2 )  <> ("%5D" :: Text)

        resp <- unsafeRequest ("GET", endpoint) $ Nothing
        verify (resp :: Either ClientError [Wallet])
            [ expectSuccess
            , expectListSizeEqual 3
            , expectListItemFieldEqual 0 walletId ( Client.WalletId $ walletIds !! 2 )
            , expectListItemFieldEqual 1 walletId ( Client.WalletId $ walletIds !! 1 )
            , expectListItemFieldEqual 2 walletId ( Client.WalletId $ walletIds !! 0 )
            ]

    scenario "WALLETS_LIST_03 - One can filter wallets by wallet id -- IN[value]" $ do
        -- IN[a,b,c,d] : allow objects with the attribute belonging to one provided.

        fixtures <- forM ([1,2,3]) $ \name -> do
            setup $ defaultSetup
                & walletName .~ show (name :: Int)

        let walletIds =
                     [ fromWalletId $ (fixtures !! 0) ^. wallet . walletId
                     , fromWalletId $ (fixtures !! 1) ^. wallet . walletId
                     , fromWalletId $ (fixtures !! 2) ^. wallet . walletId
                     ]

        let endpoint = "api/v1/wallets?sort_by=created_at&id=IN%5B" <> ( walletIds !! 0 )
                        <> "," <> ( walletIds !! 2 )  <> ("%5D" :: Text)

        resp <- unsafeRequest ("GET", endpoint) $ Nothing

        verify (resp :: Either ClientError [Wallet])
            [ expectSuccess
            , expectListSizeEqual 2
            , expectListItemFieldEqual 0 walletId ( Client.WalletId $ walletIds !! 2 )
            , expectListItemFieldEqual 1 walletId ( Client.WalletId $ walletIds !! 0 )
            ]

    describe "WALLETS_LIST_04 - One can sort results only by 'balance' and 'created_at'" $ do

        let matrix =
                [ ( "api/v1/wallets?sort_by=created_at"
                  , [ expectSuccess
                    , expectListSizeEqual 3
                    , expectListItemFieldEqual 0 walletName "3"
                    , expectListItemFieldEqual 1 walletName "2"
                    , expectListItemFieldEqual 2 walletName "1"
                    , expectListItemFieldEqual 0 amount 1
                    , expectListItemFieldEqual 1 amount 2
                    , expectListItemFieldEqual 2 amount 3
                    ]
                  )
                , ( "api/v1/wallets?sort_by=DES%5Bcreated_at%5D"
                  , [ expectSuccess
                    , expectListSizeEqual 3
                    , expectListItemFieldEqual 0 walletName "3"
                    , expectListItemFieldEqual 1 walletName "2"
                    , expectListItemFieldEqual 2 walletName "1"
                    , expectListItemFieldEqual 0 amount 1
                    , expectListItemFieldEqual 1 amount 2
                    , expectListItemFieldEqual 2 amount 3
                    ]
                  )
                , ( "api/v1/wallets?sort_by=ASC%5Bcreated_at%5D"
                  , [ expectSuccess
                    , expectListSizeEqual 3
                    , expectListItemFieldEqual 0 walletName "1"
                    , expectListItemFieldEqual 1 walletName "2"
                    , expectListItemFieldEqual 2 walletName "3"
                    , expectListItemFieldEqual 0 amount 3
                    , expectListItemFieldEqual 1 amount 2
                    , expectListItemFieldEqual 2 amount 1
                    ]
                  )
                , ( "api/v1/wallets?sort_by=balance"
                  , [ expectSuccess
                    , expectListSizeEqual 3
                    , expectListItemFieldEqual 0 walletName "1"
                    , expectListItemFieldEqual 1 walletName "2"
                    , expectListItemFieldEqual 2 walletName "3"
                    , expectListItemFieldEqual 0 amount 3
                    , expectListItemFieldEqual 1 amount 2
                    , expectListItemFieldEqual 2 amount 1
                    ]
                  )
                , ( "api/v1/wallets?sort_by=DES%5Bbalance%5D"
                  , [ expectSuccess
                    , expectListSizeEqual 3
                    , expectListItemFieldEqual 0 walletName "1"
                    , expectListItemFieldEqual 1 walletName "2"
                    , expectListItemFieldEqual 2 walletName "3"
                    , expectListItemFieldEqual 0 amount 3
                    , expectListItemFieldEqual 1 amount 2
                    , expectListItemFieldEqual 2 amount 1
                    ]
                  )
                , ( "api/v1/wallets?sort_by=ASC%5Bbalance%5D"
                  , [ expectSuccess
                    , expectListSizeEqual 3
                    , expectListItemFieldEqual 0 walletName "3"
                    , expectListItemFieldEqual 1 walletName "2"
                    , expectListItemFieldEqual 2 walletName "1"
                    , expectListItemFieldEqual 0 amount 1
                    , expectListItemFieldEqual 1 amount 2
                    , expectListItemFieldEqual 2 amount 3
                    ]
                  )
                , ( "api/v1/wallets?sort_by=漢patate字"
                  , [ expectSuccess
                    , expectListSizeEqual 3
                    ]
                  )

                ]

        forM_ matrix $ \(endpoint, expectations) -> scenario endpoint $ do
            pendingWith "Test fails due to bug #220"
            forM_ (zip [1,2,3] [3,2,1]) $ \(name, coins) -> do
                setup $ defaultSetup
                    & walletName .~ show (name :: Int)
                    & initialCoins .~ [coins]

            resp <- unsafeRequest ("GET", fromString endpoint) $ Nothing
            verify (resp :: Either ClientError [Wallet]) expectations

    scenario "WALLETS_UPDATE_01 - updating a wallet persists the update" $ do
        fixture <- setup defaultSetup

        [_, response01] <- sequence
            [ request $ Client.updateWallet
                $- (fixture ^. wallet . walletId)
                $- WalletUpdate StrictAssurance "漢patate字"
            , request $ Client.getWallet
                $- (fixture ^. wallet . walletId)
            ]

        verify response01
            [ expectFieldEqual walletName "漢patate字"
            , expectFieldEqual assuranceLevel StrictAssurance
            ]

    scenario "WALLETS_UPDATE_PASS_04 - updating password updates date in spendingPasswordLastUpdate" $ do
        fixture <- setup $ defaultSetup
            & rawPassword .~ "3132333435363738393031323334353637383930313233343536373839303030"

        successfulRequest $ Client.deleteWallet
            $- (fixture ^. wallet . walletId)

        -- 1. Create a wallet with init spending password.
        response <- request $ Client.postWallet $- NewWallet
            (fixture ^. backupPhrase)
            (Just $ fixture ^. spendingPassword)
            NormalAssurance
            "My new Wallet with spending password"
            CreateWallet

        -- 2. Verify that we have a spending password on new wallet.
        verify response
            [ expectFieldEqual hasSpendingPassword True
            ]

        -- 3. Remove spending password.
        updatePasswordResp <- request $ Client.updateWalletPassword
            $- (fixture ^. wallet . walletId)
            $- PasswordUpdate (fixture ^. spendingPassword) mempty

        -- 4. Verify there's no spending password anymore and spendingPasswordLastUpdate was changed.
        -- After wallet was created, spendingPasswordLastUpdate contains time of creation. Of course,
        -- spendingPasswordLastUpdate after update is a little bit later, so verify it.
        let latestUpdateTime = fixture ^. wallet ^. spendingPasswordLastUpdate
        verify updatePasswordResp
            [ expectFieldEqual hasSpendingPassword False
            , expectFieldDiffer spendingPasswordLastUpdate latestUpdateTime
            ]

    scenario "WALLETS_UTXO_03 - UTxO statistics reflect wallet's inactivity" $ do
        fixture <- setup defaultSetup

        response <- request $ Client.getUtxoStatistics
            $- (fixture ^. wallet . walletId)

        verify response
            [ expectWalletUTxO []
            ]

    scenario "WALLETS_UTXO_04 - UTxO statistics reflect wallet's activity" $ do
        fixture <- setup $ defaultSetup
            & initialCoins .~ [14, 42, 1337]

        response <- request $ Client.getUtxoStatistics
            $- (fixture ^. wallet . walletId)

        verify response
            [ expectWalletUTxO [14, 42, 1337]
            ]

    -- Below are scenarios that are somewhat 'symmetric' for both 'create' and 'restore' operations.
    forM_ [CreateWallet, RestoreWallet] $ \operation -> describe (show operation) $ do
        scenario "WALLETS_CREATE_01 - One can create/restore previously deleted wallet" $ do
            fixture <- setup $ defaultSetup
                & initialCoins .~ [1000111]

            successfulRequest $ Client.deleteWallet
                $- (fixture ^. wallet . walletId)

            response <- request $ Client.postWallet $- NewWallet
                (fixture ^. backupPhrase)
                noSpendingPassword
                StrictAssurance
                kanjiPolishWalletName
                operation

            verify response
                [ expectWalletEventuallyRestored
                , expectFieldEqual walletName kanjiPolishWalletName
                , expectFieldEqual assuranceLevel StrictAssurance
                , expectFieldEqual hasSpendingPassword False
                , expectFieldEqual amount $ case operation of
                    RestoreWallet -> 1000111
                    CreateWallet  -> 0
                ]


        scenario "WALLETS_CREATE_02 - One can create/restore wallet without spending password" $ do
            fixture <- setup $ defaultSetup

            successfulRequest $ Client.deleteWallet
                $- (fixture ^. wallet . walletId)

            restoreResp <- request $ Client.postWallet $- NewWallet
                (fixture ^. backupPhrase)
                noSpendingPassword
                StrictAssurance
                kanjiPolishWalletName
                operation

            verify restoreResp
                [ expectWalletEventuallyRestored
                , expectFieldEqual walletName kanjiPolishWalletName
                , expectFieldEqual assuranceLevel StrictAssurance
                , expectFieldEqual hasSpendingPassword False
                ]


        scenario "WALLETS_CREATE_03 - One can create/restore wallet with spending password" $ do
            fixture <- setup $ defaultSetup
                & rawPassword .~ "patate"

            successfulRequest $ Client.deleteWallet
                $- (fixture ^. wallet . walletId)

            response <- request $ Client.postWallet $- NewWallet
                (fixture ^. backupPhrase)
                (Just $ fixture ^. spendingPassword)
                NormalAssurance
                "My HODL Wallet"
                operation

            verify (response :: Either ClientError Wallet)
                [ expectFieldEqual hasSpendingPassword True
                , expectFieldEqual walletName "My HODL Wallet"
                , expectFieldEqual assuranceLevel NormalAssurance
                ]


        describe "WALLETS_CREATE_04 - One cannot create/restore wallet if spending password is not hex-encoded string" $ do
            let matrix =
                    [ ( "spending password is less than 32 bytes / 64 characters"
                      , [json| "5416b2988745725998907addf4613c9b0764f04959030e1b81c6" |]
                      , [ expectJSONError "Error in $.spendingPassword: Expected spending password to be of either length 0 or 32, not 26" ]
                      )
                    , ( "spending password is more than 32 bytes / 64 characters"
                      , [json| "c0b75cebcd14403d7abba4227cea5b99b1b09148623cd927fa7bb40c6cca5583c" |]
                      , [ expectJSONError "Error in $.spendingPassword: suffix is not in base-16 format: c" ]
                      )
                    , ( "spending password is a number"
                      , [json| 541 |]
                      , [ expectJSONError "Error in $.spendingPassword: expected parseJSON failed for PassPhrase, encountered Number" ]
                      )
                    , ( "spending password is an empty string"
                      , [json| " " |]
                      , [ expectJSONError "Error in $.spendingPassword: suffix is not in base-16 format" ]
                      )
                    , ( "spending password is an array"
                      , [json| [] |]
                      , [ expectJSONError "Error in $.spendingPassword: expected parseJSON failed for PassPhrase, encountered Array" ]
                      )
                    , ( "spending password is an arbitrary utf-8 string"
                      , [json| "patate" |]
                      , [ expectJSONError "Error in $.spendingPassword: suffix is not in base-16 format" ]
                      ) -- ^ is an arbitrary string (not hex-encoded)
                    ]
            forM_ matrix $ \(title, password, expectations) -> scenario title $ do
                response <- unsafeRequest ("POST", "api/v1/wallets") $ Just $ [json|{
                    "operation": #{operation},
                    "backupPhrase": #{testBackupPhrase},
                    "assuranceLevel": "normal",
                    "name": "MyFirstWallet",
                    "spendingPassword": #{password}
                }|]
                verify (response :: Either ClientError Wallet) expectations


        scenario "WALLETS_CREATE_05 - One cannot create/restore wallet that already exists" $ do
            fixture <- setup $ defaultSetup

            response <- request $ Client.postWallet $- NewWallet
                (fixture ^. backupPhrase)
                noSpendingPassword
                defaultAssuranceLevel
                defaultWalletName
                CreateWallet

            verify response
                [ expectWalletError (WalletAlreadyExists (fixture ^. wallet . walletId))
                ]


        describe "WALLETS_CREATE_06 - One cannot create/restore wallet using less or more than 12 mnemonics which are valid BIP-39" $ do
            let matrix =
                    [ ( "less than 12 words"
                      , [json| #{mnemonicsWith9Words} |]
                      , [ expectJSONError "Invalid number of mnemonic words: got 9 words, expected 12 words" ]
                      )
                    , ( "more than 12 words"
                      , [json| #{mnemonicsWith15Words} |]
                      , [ expectJSONError "Invalid number of mnemonic words: got 15 words, expected 12 words" ]
                      )
                    , ("empty mnemonic"
                      , [json| [] |]
                      , [ expectJSONError "Invalid number of mnemonic words: got 0 words, expected 12 words" ]
                      )
                    ]
            forM_ matrix $ \(title, mnemonic, expectations) -> scenario title $ do
                response <- unsafeRequest ("POST", "api/v1/wallets") $ Just $ [json|{
                    "operation": #{operation},
                    "backupPhrase": #{mnemonic},
                    "assuranceLevel": "strict",
                    "name": #{russianWalletName},
                    "spendingPassword": "5416b2988745725998907addf4613c9b0764f04959030e1b81c603b920a115d0"
                }|]
                verify (response :: Either ClientError Wallet) expectations


        describe "WALLETS_CREATE_07 - One cannot create/restore wallet with invalid BIP-39 mnemonics" $ do
            let matrix =
                    [ ( "Kanji mnemonics / non-English mnemonics"
                      , [json| #{kanjiMnemonics} |]
                      , [ expectJSONError "Error in $.backupPhrase: MnemonicError: Invalid dictionary word:" ]
                      )
                    , ( "Invalid mnemonics"
                      , [json| #{invalidMnemonics} |]
                      , [ expectJSONError "Error in $.backupPhrase: MnemonicError: Invalid entropy checksum: got Checksum 11, expected Checksum 5" ]
                      )
                    ]
            forM_ matrix $ \(title, mnemonic, expectations) -> scenario title $ do
                response <- unsafeRequest ("POST", "api/v1/wallets") $ Just $ [json|{
                    "operation": #{operation},
                    "backupPhrase": #{mnemonic},
                    "assuranceLevel": "normal",
                    "name": #{russianWalletName},
                    "spendingPassword": "5416b2988745725998907addf4613c9b0764f04959030e1b81c603b920a115d0"
                }|]
                verify (response :: Either ClientError Wallet) expectations


        scenario "WALLET_CREATE_08 - One cannot create/restore wallet with mnemonics from the API doc" $ do
            response <- unsafeRequest ("POST", "api/v1/wallets") $ Just $ [json|{
                "operation": #{operation},
                "backupPhrase": #{apiDocsBackupPhrase},
                "assuranceLevel": "strict",
                "name": #{russianWalletName},
                "spendingPassword": "5416b2988745725998907addf4613c9b0764f04959030e1b81c603b920a115d0"
            }|]
            verify (response :: Either ClientError Wallet)
                [ expectJSONError "Error in $.backupPhrase: Forbidden Mnemonic: an example Mnemonic has been submitted. Please generate a fresh and private Mnemonic from a trusted source"
                ]


        describe "WALLETS_CREATE_09 - One cannot create/restore wallet with assurance level other than 'strict' or 'normal'" $ do
            let matrix =
                    [ ( "Arbitrary String"
                      , russianWalletName
                      , [ expectJSONError "Error in $.assuranceLevel: When parsing Cardano.Wallet.API.V1.Types.AssuranceLevel expected a String with the tag of a constructor but got" ]
                      )
                    , ( "Empty String"
                      , ""
                      , [ expectJSONError "Error in $.assuranceLevel: When parsing Cardano.Wallet.API.V1.Types.AssuranceLevel expected a String with the tag of a constructor but got" ]
                      )
                    ]
            forM_ matrix $ \(title, level, expectations) -> scenario title $ do
                response <- unsafeRequest ("POST", "api/v1/wallets") $ Just $ [json|{
                    "operation": #{operation},
                    "backupPhrase": #{testBackupPhrase},
                    "assuranceLevel": #{level},
                    "name": #{russianWalletName},
                    "spendingPassword": "5416b2988745725998907addf4613c9b0764f04959030e1b81c603b920a115d0"
                }|]
                verify (response :: Either ClientError Wallet) expectations


        describe "WALLETS_CREATE_10 - One cannot create/restore without all required parameters (operation)" $ do
            let matrix =
                    [ ( "Missing operation"
                      , [json|{
                            "backupPhrase": #{testBackupPhrase},
                            "assuranceLevel": "normal",
                            "name": "my hodl wallet",
                            "spendingPassword": "5416b2988745725998907addf4613c9b0764f04959030e1b81c603b920a115d0"
                        }|]
                      , [ expectJSONError "When parsing the record newWallet of type Cardano.Wallet.API.V1.Types.NewWallet the key operation was not present." ]
                      )
                    , ( "Missing backupPhrase"
                      , [json|{
                            "operation": #{operation},
                            "assuranceLevel": "normal",
                            "name": "my hodl wallet",
                            "spendingPassword": "5416b2988745725998907addf4613c9b0764f04959030e1b81c603b920a115d0"
                        }|]
                      , [ expectJSONError "When parsing the record newWallet of type Cardano.Wallet.API.V1.Types.NewWallet the key backupPhrase was not present." ]
                      )
                    , ( "Missing assuranceLevel"
                      , [json|{
                            "operation": #{operation},
                            "backupPhrase": #{testBackupPhrase},
                            "name": "my hodl wallet",
                            "spendingPassword": "5416b2988745725998907addf4613c9b0764f04959030e1b81c603b920a115d0"
                        }|]
                      , [ expectJSONError "When parsing the record newWallet of type Cardano.Wallet.API.V1.Types.NewWallet the key assuranceLevel was not present." ]
                      )
                    , ( "Missing name"
                      , [json|{
                            "operation": #{operation},
                            "backupPhrase": #{testBackupPhrase},
                            "assuranceLevel": "normal",
                            "spendingPassword": "5416b2988745725998907addf4613c9b0764f04959030e1b81c603b920a115d0"
                        }|]
                      , [ expectJSONError "When parsing the record newWallet of type Cardano.Wallet.API.V1.Types.NewWallet the key name was not present." ]
                      )
                    ]
            forM_ matrix $ \(title, body, expectations) -> scenario title $ do
                response <- unsafeRequest ("POST", "api/v1/wallets") $ Just body
                verify (response :: Either ClientError Wallet) expectations


        describe "WALLETS_CREATE_11 - One can create/restore wallet with name of any length" $ do
            let matrix =
                    [ ("long wallet name", longWalletName)
                    , ("empty name", "")
                    ]
            forM_ matrix $ \(title, name) -> scenario title $ do
                fixture <- setup $ defaultSetup
                    & walletName .~ name
                response <- request $ Client.getWallet
                    $- (fixture ^. wallet . walletId)
                verify response
                    [ expectFieldEqual walletName name
                    ]

    describe "WALLETS_CREATE_12 - Cannot perform operation other than 'create' or 'restore'" $ do
        let matrix =
                [ ( "Invalid operation name"
                  , [json| #{russianWalletName} |]
                  , [ expectJSONError "Error in $.operation: When parsing Cardano.Wallet.API.V1.Types.WalletOperation expected a String with the tag of a constructor but got" ]
                  )
                , ( "Empty operation name"
                  , [json| "" |]
                  , [ expectJSONError "Error in $.operation: When parsing Cardano.Wallet.API.V1.Types.WalletOperation expected a String with the tag of a constructor but got" ]
                  )
                ]
        forM_ matrix $ \(title, operation, expectations) -> scenario title $ do
            response <- unsafeRequest ("POST", "api/v1/wallets") $ Just $ [json|{
                "operation": #{operation},
                "backupPhrase": #{testBackupPhrase},
                "assuranceLevel": "normal",
                "name": #{russianWalletName},
                "spendingPassword": "5416b2988745725998907addf4613c9b0764f04959030e1b81c603b920a115d0"
            }|]
            verify (response :: Either ClientError Wallet) expectations
  where

    fromWalletId :: Client.WalletId -> Text
    fromWalletId (Client.WalletId a) = a

    testBackupPhrase :: [Text]
    testBackupPhrase =
        ["clap", "panda", "slim", "laundry", "more", "vintage", "cash", "shaft"
        , "token", "history", "misery", "problem"]

    apiDocsBackupPhrase :: [Text]
    apiDocsBackupPhrase =
        ["squirrel", "material", "silly", "twice", "direct", "slush", "pistol", "razor"
         , "become", "junk", "kingdom", "flee"]

    invalidMnemonics :: [Text]
    invalidMnemonics =
        ["clinic","nuclear","paddle","leg","lounge","fabric","claw","trick"
        ,"divide","pretty","argue","master"]

    kanjiMnemonics :: [Text]
    kanjiMnemonics =
        ["けぬき", "つごう", "ちかい",　"いみん",　"よしゅう",　"ついたち",　"だっかい"
        ,　"くたびれる",　"はんとし",　"けしょう",　"すうじつ",　"てそう"]

    mnemonicsWith9Words :: [Text]
    mnemonicsWith9Words =
        ["pave", "behind", "simple", "lobster", "digital", "ready", "switch"
        , "uncle", "dragon"]

    mnemonicsWith15Words :: [Text]
    mnemonicsWith15Words =
        ["organ", "uniform", "anchor", "exhibit", "satisfy", "scrub", "vacant"
        , "hold", "spawn", "super", "tenant", "change", "illegal", "yard", "quarter"]

    kanjiPolishWalletName :: Text
    kanjiPolishWalletName = "亜哀愛悪握圧扱安暗案以位依偉囲委 威尉意慰易為異移維緯胃衣違遺医井域育一壱逸稲芋印員因 姻引飲院陰隠韻右宇羽雨\n渦浦運雲営影映栄永泳英衛詠鋭液疫益駅悦 謁越閲円園宴延援沿演炎煙猿縁遠鉛塩 汚凹央奥往応押横欧殴王翁黄沖億屋憶乙卸恩温穏音下化仮何価佳加可夏嫁家寡科暇果架歌河火禍稼箇花荷華菓課貨過蚊我画芽賀雅餓介会解回塊壊快怪悔懐戒拐改械海灰界皆絵開階貝劾外害慨概涯街該垣嚇各拡格核殻獲確穫覚角較郭閣隔革学岳楽額掛潟割喝括活渇滑褐轄且株刈乾冠寒刊勘勧巻喚堪完官寛干幹患感慣憾換敢棺款歓汗漢環甘監看管簡緩缶肝艦観貫還鑑間閑関陥館丸含岸眼岩頑顔願企危喜器基奇寄岐希幾忌揮机旗既期棋棄機帰気汽祈季紀規記貴起軌輝飢騎鬼偽儀宜戯技擬欺犠疑義議菊吉喫詰却客脚虐逆丘久休及吸宮弓急救朽求泣球究窮級糾給旧牛去居巨拒拠\n挙虚許距漁魚享京供競共凶協叫境峡強恐恭挟教橋況狂狭矯胸脅興郷鏡響驚仰凝暁業局曲極玉勤均斤琴 禁筋緊菌襟謹近金吟銀九句区苦駆具愚虞空偶遇隅屈掘靴繰桑勲君薫訓群軍郡係傾刑兄啓型契形径恵慶憩掲携敬景渓系経継茎蛍計警軽鶏芸迎鯨劇撃激傑欠決潔穴結血月件倹健兼券剣圏堅嫌建憲懸検権犬献研絹県肩見謙賢軒遣険顕験元原厳幻弦減源玄現言限個古呼固孤己庫弧戸故枯湖誇雇顧鼓五互午呉娯後御悟碁語誤護交侯候光公功効厚口向后坑好孔孝工巧幸広康恒慌抗拘控攻更校構江洪港溝甲皇硬稿紅絞綱耕考肯航荒行衡講貢購郊酵鉱鋼降項香高剛号合拷豪克刻告国穀酷黒獄腰骨込今困墾婚恨懇昆根混紺魂佐唆左差査砂詐鎖座債催再最妻宰彩才採栽歳済災砕祭斎細菜裁載際剤在材罪財坂咲崎作削搾昨策索錯桜冊刷察撮擦札殺雑皿三傘参山惨散桟産算蚕賛酸暫残仕伺 使刺司史嗣四士始姉姿子市師志思指支施旨枝止死氏祉私糸紙紫肢脂至視詞詩試誌諮資賜雌飼歯事似侍児字寺慈持時次滋治璽磁示耳自辞式識軸七執失室湿漆疾質実芝舎写射捨赦斜煮社者謝車遮蛇邪借勺尺爵酌釈若寂弱主取守手朱殊狩珠種趣酒首儒受寿授樹需囚収周宗就州修愁拾秀秋終習臭舟衆襲週酬集醜住充十従柔汁渋獣縦重銃叔宿淑祝縮粛塾熟出術述俊春瞬准循旬殉準潤盾純巡遵順処初所暑庶緒署書諸助叙女序徐除傷償勝匠升召商唱奨宵将小少尚床彰承抄招掌昇昭晶松沼消渉焼焦照症省硝礁祥\r\n称章笑粧紹肖衝訟証詔詳象賞鐘障上丈乗冗剰城場壌嬢常\n情条浄状畳蒸譲醸錠嘱飾植殖織職ęó ąśł żźćń色触食辱伸信侵唇娠寝審心慎振新森浸深申真神紳臣薪親診身辛進針震人仁刃尋甚 尽迅陣酢図吹垂帥推水炊睡粋衰遂酔錘随髄崇数枢据杉澄寸世瀬畝是制勢姓征性成政整星晴正清牲生盛精聖声製西誠誓請逝青静斉税隻席惜斥昔析石積籍績責赤跡切拙接摂折設窃節説雪絶舌仙先千占宣専川戦扇栓泉浅洗染潜旋線繊船薦践選遷銭銑鮮前善漸然全禅繕塑措疎礎祖租粗素組訴阻僧創双倉喪壮奏層想捜掃挿操早曹巣槽燥争相窓総草荘葬藻装走送遭霜騒像増憎臓蔵贈造促側則即息束測足速俗属賊族続卒存孫尊損村他多太堕妥惰打駄体対耐帯待怠態替泰滞胎袋貸退逮隊代台大第題滝卓宅択拓沢濯託濁諾但達奪脱棚谷丹単嘆担探淡炭短端胆誕鍛団壇弾断暖段男談値知地恥池痴稚置致遅築畜竹蓄逐秩窒茶嫡着中仲宙忠抽昼柱注虫衷鋳駐著貯丁兆帳庁弔張彫徴懲挑朝潮 町眺聴脹腸調超跳長頂鳥勅直朕沈珍賃鎮陳津墜追痛通塚漬坪釣亭低停偵貞呈堤定帝底庭廷弟抵提程締艇訂逓邸泥摘敵滴的笛適哲徹撤迭鉄典天展店添転点伝殿田電吐塗徒斗渡登途都努度土奴怒倒党冬凍刀唐塔島悼投搭東桃棟盗湯灯当痘等答筒糖統到討謄豆踏逃透陶頭騰闘働動同堂導洞童胴道銅峠匿得徳特督篤毒独読凸突届屯豚曇鈍内縄南軟難二尼弐肉日乳入如尿任妊忍認寧猫熱年念燃粘悩濃納能脳農把覇波派破婆馬俳廃拝排敗杯背肺輩配倍培媒梅買売賠陪伯博拍泊白舶薄迫漠爆 縛麦箱肌畑八鉢発髪伐罰\n抜閥伴判半反帆搬板版犯班畔繁般藩販範煩頒飯晩番盤蛮卑否妃彼悲扉批披比泌疲皮碑秘罷肥 被費避非飛備尾微美鼻匹必筆姫百俵標氷漂票表評描病秒苗品浜貧賓頻敏 瓶不付夫婦富布府怖扶敷普浮父符腐膚譜負賦赴附侮武舞部封風伏副復幅服福腹複覆払沸仏物分噴墳憤奮粉紛雰文聞丙併兵塀幣平弊柄並閉陛米壁癖別偏変片編辺返遍便勉弁保舗捕歩補穂募墓慕暮母簿倣俸包報奉宝峰崩抱放方法泡砲縫胞芳褒訪豊邦飽乏亡傍剖坊妨帽忘忙房暴望某棒冒紡肪膨謀貿防北僕墨撲朴牧没堀奔本翻凡盆摩磨魔麻埋妹枚毎幕膜又抹末繭万慢満漫味未魅岬密脈妙民眠務夢無矛霧婿名命明盟迷銘鳴滅免綿面模茂妄毛猛盲網耗木黙目戻問紋門匁夜野矢厄役約薬訳躍柳愉油癒諭輸唯優勇友幽悠憂有猶由裕誘遊郵雄融夕予余与誉預幼容庸揚揺擁曜様洋溶用窯羊葉要謡踊陽養抑欲浴翌翼羅裸来頼雷絡落酪乱卵欄濫覧利吏履理痢裏里離陸律率立略流留硫粒隆 竜慮旅虜了僚両寮料涼猟療糧良量陵領力緑倫厘林臨輪隣塁涙累類令例冷励礼鈴隷零霊麗齢暦歴列劣烈裂廉恋練連錬炉路露労廊朗楼浪漏老郎六録論和話賄惑枠湾腕"

    longWalletName :: Text
    longWalletName = mconcat $ replicate 1000 kanjiPolishWalletName

    russianWalletName :: Text
    russianWalletName = "АаБбВвГгДдЕеЁёЖжЗзИиЙйКкЛлМмНнОоПпРрСсТтУуФфХхЦцЧчШшЩщЪъЫыЬьЭэЮюЯяІѢѲѴѵѳѣі"
