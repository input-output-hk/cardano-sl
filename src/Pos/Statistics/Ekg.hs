{-# LANGUAGE ApplicativeDo #-}

module Pos.Statistics.Ekg
       ( EkgParams (..)
       , ekgParamsOption
       ) where

import           Universum
import qualified Options.Applicative.Simple as Opt
import           Serokell.Util.OptParse     (fromParsec)
import           Pos.Util.TimeWarp          (NetworkAddress, addrParserNoWildcard)

data EkgParams = EkgParams
    { ekgHost :: !ByteString
    , ekgPort :: !Int
    } deriving (Show)

ekgParamsOption :: Opt.Parser EkgParams
ekgParamsOption = do
    addr <- ekgServerOption
    pure $ EkgParams
        { ekgHost = fst addr
        , ekgPort = fromIntegral (snd addr)
        }

ekgServerOption :: Opt.Parser NetworkAddress
ekgServerOption = Opt.option (fromParsec addrParserNoWildcard) $
    Opt.long "ekg-server" <>
    Opt.metavar "IP:PORT" <>
    Opt.help "Host and port for the EKG server"
