module MacInstaller where
  
import           Data.Text as T
import           System.Directory
import           System.IO.Temp (withSystemTempDirectory)
import           Turtle (procs)

-- https://developer.apple.com/library/content/documentation/DeveloperTools/Reference/DistributionDefinitionRef/Chapters/Introduction.html

main :: IO ()
main = withSystemTempDirectory "mac-cardano-installer" $ \dir -> do
  -- TODO: copy over the binary
  let pkgargs =
       [ "--identifier"
       , "org.cardano.binaries.pkg"
       , "--install-location"
       , "/usr/local/bin"
       --"--scripts" $pkg_scripts 
       --"--filter 'Scripts.*' 
       , "--root"
       , (T.pack dir)
       , "binaries.pkg"
       ]
  procs "pkgbuild" pkgargs mempty
  let productargs =
       [ "--distribution"
       , "data/Distribution.xml"
       , "--package-path"
       , "."
       , "--resources"
       , "data/Resources"
       , "Cardano.pkg"
       ]
  procs "productbuild" productargs mempty
