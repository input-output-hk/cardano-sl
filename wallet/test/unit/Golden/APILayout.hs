{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NumDecimals          #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Golden.APILayout (spec) where

import           Universum

import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Text.Encoding as Text
import           Servant
import           Test.Hspec


import qualified Cardano.Wallet.API.V1 as V1

-- Our API apparently is returning JSON Arrays which is considered bad practice as very old
-- browsers can be hacked: https://haacked.com/archive/2009/06/25/json-hijacking.aspx/
-- The general consensus, after discussing this with the team, is that we can be moderately safe.
-- stack test cardano-sl-wallet-new --fast --test-arguments '-m "Servant API Properties"'
spec :: Spec
spec = do
    describe "Servant Layout" $ do
        let layoutPath = "./test/unit/Golden/golden/api-layout.txt"
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

serverLayout :: ByteString
serverLayout = Text.encodeUtf8 (layout (Proxy @V1.API))
