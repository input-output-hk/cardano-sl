#!/usr/bin/env stack
-- stack runghc --package universum --package lens --package lens-aeson --package time --package cassava --package split --package text --package fmt --package directory --package filepath --package megaparsec

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

import           Universum
import           Unsafe

import qualified Control.Lens as L
import qualified Data.Aeson.Lens as L
import qualified Data.Csv as CSV
import           Data.List (isInfixOf)
import qualified Data.List.Split as Split
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Time as Time
import           Fmt (Buildable (..), genericF)
import qualified System.Directory as Dir
import           System.FilePath (takeFileName, (</>))
import qualified Text.Megaparsec as P
import           Text.Megaparsec.Text ()

-- Usage: Give it path to the directory with logs
main :: IO ()
main = do
    [logsDir] <- getArgs
    logs <- readLogFiles logsDir
    putStrLn . TL.decodeUtf8 $
        CSV.encodeDefaultOrderedByName (concatMap processLog logs)

type Log = (FilePath, (LText, LText), LText)

-- | Each tuple contains directory, client.info & payload.json, and log.
readLogFiles :: FilePath -> IO [Log]
readLogFiles dir = do
    ds <- map (dir </>) <$> Dir.listDirectory dir
    fmap catMaybes $ mapM readLogDir ds

readLogDir :: FilePath -> IO (Maybe Log)
readLogDir dir = do
    fs <- Dir.listDirectory dir
    clientInfo <- if "client.info" `elem` fs
        then Just <$> TL.readFile (dir </> "client.info")
        else pure Nothing
    payload <- if "payload.json" `elem` fs
        then Just <$> TL.readFile (dir </> "payload.json")
        else pure Nothing
    logFile <- case find ("main" `isPrefixOf`) fs of
        Just f  -> Just <$> TL.readFile (dir </> f)
        Nothing -> pure Nothing
    pure $ (takeFileName dir,,)
           <$> ((,) <$> clientInfo <*> payload)
           <*> logFile

-- | Extract IP, OS, and version from JSON files.
getClientInfo :: (LText, LText) -> Maybe (Text, Text, Text)
getClientInfo (info, payload) =
    (,,) <$> (info    ^? strKey "addr" . L.to (T.takeWhile (/= ':')))
         <*> (payload ^? strKey "os")
         <*> (payload ^? strKey "version")
  where
    strKey k = L.key k . L._String

data Event = NodeStart | WalletStart | Adoption Count
  deriving (Eq, Show, Generic)

instance Buildable Event where
  build = genericF

data Count = One | Many
  deriving (Eq, Show, Generic)

instance Buildable Count where
  build = genericF

type Line = (Time.UTCTime, Event)

parseLine :: Text -> Maybe Line
parseLine = P.parseMaybe @P.Dec $ do
    let inside a b = P.char a >> P.manyTill P.anyChar (P.char b)
    inside '[' ']' >> P.space
    time <- maybe empty pure . readMaybe =<< inside '[' ']'
    msg <- P.manyTill P.anyChar P.eof
    if | "Generated dht key" `isInfixOf` msg ->
             pure (time, NodeStart)
       | "DAEDALUS has STARTED" `isInfixOf` msg ->
             pure (time, WalletStart)
       | "Blocks have been adopted" `isInfixOf` msg ->
             pure (time, Adoption Many)
       | "Block has been adopted" `isInfixOf` msg ->
             pure (time, Adoption One)
       | otherwise ->
             P.parserFail "unknown event type"

type Run = [Line]

getRuns :: [Line] -> [Run]
getRuns = Split.split
    (Split.dropInitBlank $
     Split.keepDelimsL $
     Split.whenElt ((== NodeStart) . snd))

getWalletStartingTime :: Run -> Maybe Time.NominalDiffTime
getWalletStartingTime r = do
    (t1, _) <- head r
    (t2, _) <- find ((== WalletStart) . snd) r
    pure (Time.diffUTCTime t2 t1)

getBlocksLoadingTime :: Run -> Maybe Time.NominalDiffTime
getBlocksLoadingTime r = do
    (t1, _) <- head r
    (t2, _) <- lastMay (filter ((== Adoption Many) . snd) r)
    pure (Time.diffUTCTime t2 t1)

isValidRun :: Run -> Bool
isValidRun [] = False
isValidRun (map snd -> (x:xs)) =
    and [ x == NodeStart
        , count WalletStart xs <= 1
        , dedup [a | Adoption a <- xs] `elem` [[], [One], [Many], [Many,One]]
        ]

data Entry = Entry
    { logDir             :: FilePath
    , clientIP           :: Text
    , clientOS           :: Text
    , clientVer          :: Text
    , walletStartingTime :: Maybe Time.NominalDiffTime
    , blocksLoadingTime  :: Maybe Time.NominalDiffTime
    } deriving (Eq, Show, Generic)

instance CSV.ToNamedRecord Entry
instance CSV.DefaultOrdered Entry

processLog :: Log -> [Entry]
processLog (logDir, clientInfo, logFile) = do
    (clientIP, clientOS, clientVer) <- maybeToList (getClientInfo clientInfo)
    run <- getRuns $ mapMaybe (parseLine . toText) (TL.lines logFile)
    guard (isValidRun run)
    let walletStartingTime = getWalletStartingTime run
        blocksLoadingTime  = getBlocksLoadingTime run
    pure Entry{..}

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

dedup :: Eq a => [a] -> [a]
dedup = map unsafeHead . group

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

instance CSV.ToField Time.NominalDiffTime where
    toField = CSV.toField @Double . realToFrac
