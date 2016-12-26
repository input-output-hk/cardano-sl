{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Test.NodeSpec
       ( spec
       ) where

import           Control.Concurrent.STM.TVar (newTVarIO)
import           Control.Lens                (sans, (%=), (.=))
import           Control.Monad               (forM_, unless)
import qualified Data.Set                    as S
import           Node                        (Listener (..), ListenerAction (..),
                                              SendActions (..))
import           Test.Hspec                  (Spec, describe)
import           Test.Hspec.QuickCheck       (prop)
import           Test.QuickCheck             (Property, ioProperty)
import           Test.Util                   (Parcel (..), activeWorkers, deliveryTest,
                                              expected, mkTestState, modifyTestState,
                                              newWork, addFail)


spec :: Spec
spec = describe "Node" $
    describe "delivery (client -> server)" $ do
        prop "plain" $
            plainDeliveryTest
        prop "header-filtering" $
            headersFilteringDeliveryTest

plainDeliveryTest
    :: [Parcel]
    -> Property
plainDeliveryTest parcels = ioProperty $ do
    testState <- newTVarIO mkTestState

    -- wait for sender or receiver to complete
    -- TODO: make receiver stop automatically when sender finishes
    modifyTestState testState $ do
        activeWorkers .= 1
        expected .= S.fromList parcels

    let worker peerId sendActions = newWork testState "client" $
            forM_ parcels $ sendTo sendActions peerId "ping" ()

        listener = Listener "ping" $ ListenerActionOneMsg $
            \_ _ parcel -> modifyTestState testState $ expected %= sans parcel

    deliveryTest testState [worker] [listener] Nothing

headersFilteringDeliveryTest
    :: [Parcel]
    -> Property
headersFilteringDeliveryTest parcels = ioProperty $ do
    testState <- newTVarIO mkTestState

    modifyTestState testState $ do
        activeWorkers .= 1
        expected .= S.fromList (filter toProcess parcels)

    let worker peerId sendActions = newWork testState "client" $
            forM_ parcels $
                \parcel -> sendTo sendActions peerId "ping" (toProcess parcel) parcel

        listener = Listener "ping" $ ListenerActionOneMsg $
            \_ _ parcel -> do
                unless (toProcess parcel) $ addFail testState
                    "received message which should be discarded by prelistener"
                modifyTestState testState $ expected %= sans parcel

        prelistener header _ = return header

    deliveryTest testState [worker] [listener] (Just prelistener)
