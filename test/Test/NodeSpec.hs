{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections       #-}

module Test.NodeSpec
       ( spec
       ) where

import           Control.Concurrent.STM.TVar (newTVarIO)
import           Control.Lens                (sans, (%=), (.=), (<&>))
import           Control.Monad               (unless)
import           Data.Foldable               (for_)
import qualified Data.Set                    as S
import           Node                        (Listener (..))
import           Test.Hspec                  (Spec, describe)
import           Test.Hspec.QuickCheck       (prop)
import           Test.QuickCheck             (Property, ioProperty)
import           Test.Util                   (Parcel (..), TalkStyle (..), activeWorkers,
                                              addFail, deliveryTest, expected,
                                              mkTestState, modifyTestState, newWork,
                                              receiveAll, sendAll)

spec :: Spec
spec = describe "Node" $
    describe "delivery (client -> server)" $
        for_ [SingleMessageStyle, ConversationStyle] $
        \talkStyle -> describe (show talkStyle) $ do
            prop "plain" $
                plainDeliveryTest talkStyle
            prop "header-filtering" $
                headersFilteringDeliveryTest talkStyle

plainDeliveryTest
    :: TalkStyle
    -> [Parcel]
    -> Property
plainDeliveryTest talkStyle parcels = ioProperty $ do
    testState <- newTVarIO mkTestState

    -- wait for sender or receiver to complete
    -- TODO: make receiver stop automatically when sender finishes
    modifyTestState testState $ do
        activeWorkers .= 1
        expected .= S.fromList parcels

    let worker peerId sendActions = newWork testState "client" $
            sendAll talkStyle sendActions peerId "ping" $ ((), ) <$> parcels

        listener = Listener "ping" $ receiveAll talkStyle $
            \parcel -> modifyTestState testState $ expected %= sans parcel

    deliveryTest testState [worker] [listener] Nothing

headersFilteringDeliveryTest
    :: TalkStyle
    -> [Parcel]
    -> Property
headersFilteringDeliveryTest talkStyle parcels = ioProperty $ do
    testState <- newTVarIO mkTestState

    modifyTestState testState $ do
        activeWorkers .= 1
        expected .= S.fromList (filter toProcess parcels)

    let worker peerId sendActions = newWork testState "client" $
            sendAll talkStyle sendActions peerId "ping" $
                parcels <&> \p -> (toProcess p, p)

        listener = Listener "ping" $ receiveAll talkStyle $
            \parcel -> do
                unless (toProcess parcel) $ addFail testState
                    "received message which should be discarded by prelistener"
                modifyTestState testState $ expected %= sans parcel

        prelistener header _ = return header

    deliveryTest testState [worker] [listener] (Just prelistener)
