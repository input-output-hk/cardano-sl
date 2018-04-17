{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Pos.DB.SqliteBlock () where

import           Universum
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import           Pos.Core (HasConfiguration, HeaderHash, headerHash)
import           Pos.Block.Types (Blund, SerializedBlund, SlogUndo (..), Undo (..))
import           Pos.DB.Class (MonadDB (..), MonadDBRead (..), Serialized (..), SerializedBlock,
                               SerializedUndo, getBlock, getDeserialized)

data Block = Block ByteString ByteString ByteString deriving (Show)

instance FromRow Block where
  fromRow = Block <$> field <*> field <*> field

putBlunds :: NonEmpty SerializedBlund -> IO ()
putBlunds (toList -> bs) = do
  let
    go :: SerializedBlund -> IO ()
    go (block, undo) = do
      print block
      print (unSerialized undo)
  mapM_ go bs
