import           Data.Text as T
import           Data.Monoid ((<>))
import           System.Info
import           Turtle

import qualified WindowsInstaller
import qualified MacInstaller


main :: IO ()
main = do
  echo $ "Generating installer for " <> T.pack os <> "-" <> T.pack arch
  case os of
    "linux" -> fail "No installer yet"
    "darwin" -> MacInstaller.main
    "windows" -> WindowsInstaller.main
    _ -> fail "Unknown platform."
