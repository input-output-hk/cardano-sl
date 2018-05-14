{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# OPTIONS_GHC -Weverything -Wno-unsafe -Wno-type-defaults -Wno-missed-specialisations #-}

module BlockSpec (spec, main) where

import           Universum
import           Test.Hspec
import           Control.Monad.IO.Class  (liftIO)
import qualified Pos.DB       as DB
import qualified Pos.DB.Block as DB
import           Pos.Core                (MainBlock, ProtocolMagic(..), ProtocolConstants(..), Block)
import           Pos.Arbitrary.Block.Generate (generateMainBlock)
import           Pos.Binary.Class (Bi, decodeFull', serialize')
import           Pos.Core (headerHash)
import           Pos.Block.Base

pm :: ProtocolMagic
pm = ProtocolMagic 0

pc :: ProtocolConstants
pc = ProtocolConstants
    { pcK = 7
    , pcVssMaxTTL = maxBound
    , pcVssMinTTL = minBound
    }

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Block" $ do
        it "put and get" $ do
            (block1, block2) <- liftIO $ do
                env <- DB.openNodeDBs False "DB/"
                let
                    seed = 42
                    size = 4
                let
                    block :: MainBlock
                    block = generateMainBlock pm pc seed size
                    hh = headerHash block
                    go = do
                        DB.dbPutSerBlundsRealDefaultNoIndex $ one (Right block, DB.Serialized $ "notanundo")
                        DB.dbGetSerBlockRealDefault hh
                bar <- runReaderT go env
                block2 <- case bar of
                    Just b -> do
                    let
                        block :: Block
                        Right block = decodeFull' $ DB.unSerialized b
                    pure block
                    --print block
                Nothing -> do
                    print "nothing"
                    pure undefined
                DB.closeNodeDBs env
                pure (Right block, block2)
            block1 `shouldBe` block2
