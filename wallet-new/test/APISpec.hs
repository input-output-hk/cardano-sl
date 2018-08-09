{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NumDecimals          #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module APISpec (spec) where

import           Universum

import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Text.Encoding as Text
import           Servant
import           System.Directory (getCurrentDirectory, listDirectory, makeAbsolute,
                                   setCurrentDirectory)
import           Test.Hspec

import qualified Cardano.Wallet.API.V1 as V1

-- Our API apparently is returning JSON Arrays which is considered bad practice as very old
-- browsers can be hacked: https://haacked.com/archive/2009/06/25/json-hijacking.aspx/
-- The general consensus, after discussing this with the team, is that we can be moderately safe.
-- stack test cardano-sl-wallet-new --fast --test-arguments '-m "Servant API Properties"'
spec :: Spec
spec = do
    describe "Servant Layout" $ around_ withTestDirectory $ do
        let layoutPath = "./test/golden/api-layout.txt"
            newLayoutPath = layoutPath <> ".new"
        it "has not changed" $ do
            oldLayout <- BS.readFile layoutPath `catch` \(_err :: SomeException) -> pure ""
            when (oldLayout /= serverLayout) $ do
                BS.writeFile newLayoutPath serverLayout
                expectationFailure $ List.unlines
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

serverLayout :: ByteString
serverLayout = Text.encodeUtf8 (layout (Proxy @V1.API))
