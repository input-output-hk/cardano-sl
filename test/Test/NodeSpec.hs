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

import           Control.Concurrent.STM.TVar (TVar, newTVarIO)
import           Control.Lens                (sans, (%=), (.=), (<&>))
import           Control.Monad               (unless)
import           Data.Foldable               (for_)
import qualified Data.Set                    as S
import           Node                        (Listener (..))
import           Test.Hspec                  (Spec, describe)
import           Test.Hspec.QuickCheck       (prop)
import           Test.QuickCheck             (Property, ioProperty)
import           Test.Util                   (HeavyParcel (..), Parcel (..),
                                              TalkStyle (..), TestState, activeWorkers,
                                              addFail, deliveryTest, expected,
                                              mkTestState, modifyTestState, newWork,
                                              receiveAll, sendAll)

spec :: Spec
spec = describe "Node" $
    describe "delivery (client -> server)" $ do
        for_ [SingleMessageStyle, ConversationStyle] $ \talkStyle ->
            describe (show talkStyle) $ do
                prop "plain" $
                    plainDeliveryTest talkStyle
                prop "header-filtering" $
                    headersFilteringDeliveryTest talkStyle
                prop "heavy messages sent nicely" $
                    withHeavyParcels $ plainDeliveryTest talkStyle

        prop "prelistener is called even if no appropriate listener defined" $
            prelistenerTest

prepareDeliveryTestState :: [Parcel] -> IO (TVar TestState)
prepareDeliveryTestState expectedParcels = do
    testState <- newTVarIO mkTestState

    -- wait for sender or receiver to complete
    -- TODO: make receiver stop automatically when sender finishes
    modifyTestState testState $ do
        activeWorkers .= 1
        expected .= S.fromList (filter toProcess expectedParcels)
    return testState

plainDeliveryTest
    :: TalkStyle
    -> [Parcel]
    -> Property
plainDeliveryTest talkStyle parcels = ioProperty $ do
    testState <- prepareDeliveryTestState parcels

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
    testState <- prepareDeliveryTestState parcels

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

prelistenerTest :: [Parcel] -> Property
prelistenerTest parcels = ioProperty $ do
    testState <- prepareDeliveryTestState parcels

    let worker peerId sendActions = newWork testState "client" $
            sendAll SingleMessageStyle sendActions peerId "ping" $
                -- header <- parcel; parcel <- ()
                parcels <&> \p -> (p, ())

        -- header is parcel itself
        prelistener parcel _ = do
            modifyTestState testState $ expected %= sans parcel
            return True

    deliveryTest testState [worker] [] (Just prelistener)

withHeavyParcels :: ([Parcel] -> Property) -> [HeavyParcel] -> Property
withHeavyParcels testCase megaParcels = testCase (getHeavyParcel <$> megaParcels)
