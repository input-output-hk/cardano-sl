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
    "mingw32" -> WindowsInstaller.main
    _ -> fail "No installer available for this platform."
