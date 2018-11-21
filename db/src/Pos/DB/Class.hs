{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | A set of type classes which provide access to database.
--
-- 'MonadDBRead' contains reading and iterating capabilities. The
-- advantage of it is that you don't need to do any 'IO' to use it
-- which makes it suitable for pure testing.
-- 'MonadDBRead' also provides access to the Block DB.
--
-- 'MonadDB' is a superclass of 'MonadDB' and allows to modify
-- DB. Again, its purpose is to make it possible to use DB w/o IO
-- context.
--
-- 'MonadGState' contains functions to retrieve some data from
-- GState DB without knowledge of where this data is located (where in
-- code and where in DB, i. e. by which key). For example, if X wants
-- to get data maintained by Y and doesn't know about Y, it can use
-- 'MonadGState' (which is at pretty low level).
--
-- Described two classes have RocksDB implementation "DB.Rocks" and
-- pure one for testing "DB.Pure".

module Pos.DB.Class
       (
         -- * Pure
         DBTag (..)
       , DBIteratorClass (..)
       , IterType
       , MonadDBRead (..)
       , Serialized (..)
       , SerializedBlock
       , SerializedUndo
       , SerializedBlund
       , MonadBlockDBRead
       , getDeserialized
       , getBlock
       , MonadDB (..)

         -- * GState
       , MonadGState (..)
       , gsMaxBlockSize
       , gsMaxHeaderSize
       , gsMaxTxSize
       , gsMaxProposalSize
       , gsUnlockStakeEpoch
       , gsIsBootstrapEra
       ) where

import Pos.DB.Rocks
