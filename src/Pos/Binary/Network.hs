{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Provides functionality of representing `Bi` instances as correct
-- `Message`s used by time-warp.

module Pos.Binary.Network
       (
         BiP (..)
       , DHTMsgHeader (..)
       ) where

import           Control.Monad                     (when)
import           Control.Monad.Catch               (MonadThrow (..))
import           Control.TimeWarp.Rpc.Message      (HeaderData (..),
                                                    HeaderNContentData (..),
                                                    HeaderNNameData (..),
                                                    HeaderNNameNContentData (..),
                                                    HeaderNRawData (..), Message (..),
                                                    Packable (..), RawData (..),
                                                    Unpackable (..), messageName')
import qualified Data.Binary                       as Binary
import           Data.Binary.Get                   (Decoder (..), Get, pushChunk,
                                                    runGetIncremental, runGetOrFail)
import           Data.Binary.Put                   (runPut)
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as BL
import           Data.Conduit                      (Conduit, await, leftover, yield,
                                                    (=$=))
import qualified Data.Conduit.List                 as CL
import           Data.Conduit.Serialization.Binary (ParseError (..), conduitPut)
import           Universum                         hiding (yield)

import           Pos.Binary.Class                  (Bi (..))
import           Pos.DHT.Types                     (DHTData, DHTKey (..),
                                                    DHTMsgHeader (..))

----------------------------------------------------------------------------
-- THIS MODULE IS HARDLY COPY-PASTED FROM TimeWarp.Rpc.Message
-- Something should be done in terms of refactoring.
----------------------------------------------------------------------------

-- This is copy-pasted function from Message. TODO Refactor it.
conduitGet' :: MonadThrow m => Get b -> Conduit ByteString m b
conduitGet' g = start
  where
    start = do mx <- await
               case mx of
                  Nothing -> return ()
                  Just x -> do
                               go (runGetIncremental g `pushChunk` x)
    go (Done bs _ v) = do when (not $ BS.null bs) $ leftover bs
                          yield v
                          start
    go (Fail u o e)  = throwM (ParseError u o e)
    go (Partial n)   = await >>= (go . n)


data BiP = BiP
instance (Bi h, Bi r, Message r) =>
         Packable BiP (HeaderNContentData h r) where
    packMsg p = CL.map packToRaw =$= packMsg p
      where
        packToRaw (HeaderNContentData h r) =
            HeaderNRawData h . RawData . BL.toStrict . runPut $
            do put $ messageName' r
               put r

instance Bi h
      => Packable BiP (HeaderNRawData h) where
    packMsg _ = CL.map doPut =$= conduitPut
      where
        doPut (HeaderNRawData h (RawData r)) = put h >> put r

instance Bi h
      => Unpackable BiP (HeaderData h) where
    unpackMsg _ = conduitGet' $ HeaderData <$> get

instance Bi h
      => Unpackable BiP (HeaderNRawData h) where
    unpackMsg _ = conduitGet' $ HeaderNRawData <$> get <*> (RawData <$> get)

parseHeaderNNameData :: MonadThrow m => HeaderNRawData h -> m (HeaderNNameData h)
parseHeaderNNameData (HeaderNRawData h (RawData raw)) =
    case rawE of
        Left (BL.toStrict -> bs, off, err) ->
            throwM $ ParseError bs off $ "parseHeaderNNameData: " ++ err
        Right (_, _, a) -> return a
  where
    rawE = runGetOrFail (HeaderNNameData h <$> get) $ BL.fromStrict raw

parseHeaderNNameNContentData
    :: (MonadThrow m, Bi r)
    => HeaderNRawData h -> m (HeaderNNameNContentData h r)
parseHeaderNNameNContentData (HeaderNRawData h (RawData raw)) =
    case rawE of
        Left (BL.toStrict -> bs, off, err) ->
            throwM $ ParseError bs off $ "parseHeaderNNameContentData: " ++ err
        Right (bs, off, a) ->
            if BL.null bs
                then return a
                else throwM $
                     ParseError (BL.toStrict bs) off $
                     "parseHeaderNNameContentNData: unconsumed input"
  where
    rawE =
        runGetOrFail (HeaderNNameNContentData h <$> get <*> get) $
        BL.fromStrict raw

-- | TODO: don't read whole content
instance Bi h
      => Unpackable BiP (HeaderNNameData h) where
    unpackMsg p = unpackMsg p =$= CL.mapM parseHeaderNNameData

instance (Bi h, Bi r)
       => Unpackable BiP (HeaderNNameNContentData h r) where
    unpackMsg p = unpackMsg p =$= CL.mapM parseHeaderNNameNContentData

instance (Bi h, Bi r)
       => Unpackable BiP (HeaderNContentData h r) where
    unpackMsg p = unpackMsg p =$= CL.map extract
      where
        extract (HeaderNNameNContentData h _ r) = HeaderNContentData h r


-- TODO Rewrite :(((
instance Binary.Binary DHTMsgHeader

instance Bi DHTMsgHeader where
    put = Binary.put
    get = Binary.get

instance Binary.Binary DHTKey

instance Bi DHTKey where
    put = Binary.put
    get = Binary.get

instance Binary.Binary DHTData

instance Bi DHTData where
    put = Binary.put
    get = Binary.get
