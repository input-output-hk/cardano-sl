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
import           Control.Lens                (sans, (%=), (&~), (.=))
import           Data.Foldable               (for_)
import qualified Data.Set                    as S
import           Node                        (Listener (..))
import           Test.Hspec                  (Spec, describe)
import           Test.Hspec.QuickCheck       (prop)
import           Test.QuickCheck             (Property, ioProperty)
import           Test.Util                   (HeavyParcel (..), Parcel (..),
                                              TalkStyle (..), TestState, deliveryTest,
                                              expected, mkTestState, modifyTestState,
                                              newWork, receiveAll, sendAll)

spec :: Spec
spec = describe "Node" $
    -- one sender, one receiver
    describe "delivery" $ do
        for_ [SingleMessageStyle, ConversationStyle] $ \talkStyle ->
            describe (show talkStyle) $ do
                prop "plain" $
                    plainDeliveryTest talkStyle
                prop "heavy messages sent nicely" $
                    withHeavyParcels $ plainDeliveryTest talkStyle

prepareDeliveryTestState :: [Parcel] -> IO (TVar TestState)
prepareDeliveryTestState expectedParcels =
    newTVarIO $ mkTestState &~
        expected .= S.fromList (filter toProcess expectedParcels)

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

    deliveryTest testState [worker] [listener]

withHeavyParcels :: ([Parcel] -> Property) -> [HeavyParcel] -> Property
withHeavyParcels testCase megaParcels = testCase (getHeavyParcel <$> megaParcels)
