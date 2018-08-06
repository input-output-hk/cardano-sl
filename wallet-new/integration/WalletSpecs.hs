module WalletSpecs (internalWalletSpecs, externalWalletSpecs) where

import           Universum

import           Cardano.Wallet.API.V1.Errors (WalletError (WalletAlreadyExists, WalletNotFound))
import           Cardano.Wallet.Client.Http
import           Pos.Wallet.Web.Util (walletRootPKToIdText)
import           Pos.Crypto (encodeBase58PublicKey)
import           Test.Hspec

import           Util

internalWalletSpecs :: WalletClient IO -> Spec
internalWalletSpecs wc = describe "Internal Wallets" $ do
    it "Creating a wallet makes it available." $ do
        newWallet <- randomWallet CreateWallet
        Wallet{..} <- createWalletCheck wc newWallet

        void $ shouldReturnRight $ getWallet wc walId

    it "Updating a wallet persists the update" $ do
        newWallet <- randomWallet CreateWallet
        wallet <- createWalletCheck wc newWallet
        let newName = "Foobar Bazquux"
            newAssurance = NormalAssurance
        Wallet{..} <- fmap wrData $ shouldReturnRight $ updateWallet wc (walId wallet) WalletUpdate
            { uwalName = newName
            , uwalAssuranceLevel = newAssurance
            }
        walName `shouldBe` newName
        walAssuranceLevel `shouldBe` newAssurance

    it "CreateWallet with the same mnemonics rises WalletAlreadyExists error" $
        testWalletAlreadyExists wc CreateWallet

    it "RestoreWallet with the same mnemonics throws WalletAlreadyExists" $
        testWalletAlreadyExists wc RestoreWallet

    it "Can accept Unicode characters" $ do
        newWallet <- randomWallet CreateWallet
        wallet <- createWalletCheck wc newWallet

        void $ shouldReturnRight $ updateWallet wc (walId wallet) WalletUpdate
            { uwalName = "patate漢patate字patat"
            , uwalAssuranceLevel = NormalAssurance
            }

externalWalletSpecs :: WalletClient IO -> Spec
externalWalletSpecs wc = describe "External Wallets" $ do
    it "Creating an external wallet makes it available" $ do
        let (_, rootPK) = makeWalletRootKeys SixthSK
        newExtWallet <- randomExternalWalletWithPublicKey CreateWallet rootPK
        Wallet{..} <- createExternalWalletCheck wc newExtWallet
        void $ shouldReturnRight $ getWallet wc walId

    it "Delete an external wallet removes it and its accounts completely" $ do
        -- By default external wallet has one account _without_ addresses
        -- (because they didn't generated yet), so we shouldn't check addresses.
        let (_, rootPK) = makeWalletRootKeys NinthSK
        newExtWallet <- randomExternalWalletWithPublicKey CreateWallet rootPK
        let pubKeyAsText = newewalExtPubKey newExtWallet
        Wallet{..} <- createExternalWalletCheck wc newExtWallet

        void $ shouldReturnRight $ deleteExternalWallet wc pubKeyAsText
        getWallet wc walId `shouldFailWith` (ClientWalletError WalletNotFound)

    it "If external wallet with this root PK doesn't exist - it will be created" $ do
        let (_, rootPK) = makeWalletRootKeys FifthSK
            rootPKAsBase58 = encodeBase58PublicKey rootPK
            walletIdAsText = walletRootPKToIdText rootPK

        -- There's no such a wallet yet, so it will be checked and created (with restoring).
        response <- shouldReturnRight $ postCheckExternalWallet wc rootPKAsBase58
        let (WalletAndTxHistory wallet txHistory) = wrData response
            (WalletId newlyCreatedWalletId) = walId wallet

        -- Walled id derived from root PK should obviously be equal to id of newly created wallet.
        walletIdAsText `shouldBe` newlyCreatedWalletId
        -- There's no transactions history because wallet isn't ready yet.
        txHistory `shouldBe` []

    it "If external wallet with this root PK already exists - get info about it with tx history" $ do
        let (_, rootPK) = makeWalletRootKeys TenthSK
            rootPKAsBase58 = encodeBase58PublicKey rootPK
            walletIdAsText = walletRootPKToIdText rootPK

        -- Create this wallet and check it exists.
        newExtWallet <- randomExternalWalletWithPublicKey CreateWallet rootPK
        extWallet <- createExternalWalletCheck wc newExtWallet
        void $ shouldReturnRight $ getWallet wc (walId extWallet)

        -- Since such a wallet already exists - check it and return an info and transactions history.
        response <- shouldReturnRight $ postCheckExternalWallet wc rootPKAsBase58
        let (WalletAndTxHistory wallet txHistory) = wrData response
            (WalletId walletId) = walId wallet

        -- Walled id derived from root PK should obviously be equal to id of already existed wallet.
        walletIdAsText `shouldBe` walletId
        -- There's no transactions history because wallet is newly created one.
        txHistory `shouldBe` []

testWalletAlreadyExists :: WalletClient IO -> WalletOperation -> IO ()
testWalletAlreadyExists wc action = do
    newWallet1 <- randomWallet action
    preWallet2 <- randomWallet action
    let newWallet2 =
            preWallet2
                { newwalBackupPhrase = newwalBackupPhrase newWallet1
                }
    -- First wallet creation/restoration should succeed
    void $ shouldReturnRight $ postWallet wc newWallet1
    -- Second wallet creation/restoration should rise WalletAlreadyExists
    postWallet wc newWallet2 `shouldFailWith` ClientWalletError WalletAlreadyExists
