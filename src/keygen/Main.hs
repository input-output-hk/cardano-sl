import           Control.Lens        ((.~))
import qualified Data.Text           as T
import           Options.Applicative (Parser, ParserInfo, auto, execParser, fullDesc,
                                      help, helper, info, long, metavar, option, option,
                                      progDesc, short, strOption)
import           Prelude             (show)
import           Universum           hiding (show)

import           Pos.Crypto          (keyGen)
import           Pos.Util.UserSecret (takeUserSecret, usKeys, writeUserSecretRelease)

data KeygenOptions = KO
    { koPattern :: FilePath
    , koN       :: Int
    }

generateKeyfile :: FilePath -> IO ()
generateKeyfile fp = do
    sk <- snd <$> keyGen
    us <- takeUserSecret fp
    writeUserSecretRelease $ us & usKeys .~ [sk]

replace :: FilePath -> FilePath -> FilePath -> FilePath
replace a b = T.unpack . (T.replace `on` T.pack) a b . T.pack

optsParser :: Parser KeygenOptions
optsParser = KO <$>
    strOption (long "file-pattern" <>
               short 'f' <>
               metavar "PATTERN" <>
               help "Filename pattern for generated keyfiles (`{}` is a place for number)") <*>
    option auto (long "count" <>
                 short 'n' <>
                 metavar "INT" <>
                 help "Number of files to generate")

optsInfo :: ParserInfo KeygenOptions
optsInfo = info (helper <*> optsParser) $
    fullDesc `mappend` progDesc "Tool to generate keyfiles"

main :: IO ()
main = do
    KO {..} <- execParser optsInfo
    forM_ [1..koN] $ \i ->
        generateKeyfile $ replace "{}" (show i) koPattern
    print $ show koN ++ " keyfiles are generated"
