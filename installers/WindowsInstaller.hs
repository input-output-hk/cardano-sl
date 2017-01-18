module WindowsInstaller where

import           Development.NSIS
import           Turtle (echo, procs)

writeNSIS :: IO ()
writeNSIS = writeFile "daedalus.nsi" $ nsis $ do
    name "Daedalus"                  -- The name of the installer
    outFile "daedalus-win64-installer.exe"           -- Where to produce the installer
    installDir "$PROGRAMFILES\\Daedalus"   -- The default installation directory
    requestExecutionLevel Highest     

    page Directory                   -- Pick where to install
    page InstFiles                   -- Give a progress bar while installing

    section "" [Required] $ do
        setOutPath "$INSTDIR"        -- Where to install files in this section
        createDirectory "$APPDATA\\Daedalus\\DB"
        createDirectory "$APPDATA\\Daedalus\\Wallet" 
        createDirectory "$APPDATA\\Daedalus\\Logs"
        createDirectory "$APPDATA\\Daedalus\\Secrets"
        createShortcut "$DESKTOP\\Daedalus.lnk"
          [Target "$INSTDIR\\Daedalus.exe"
          , IconFile "$INSTDIR\\Daedalus.exe"
          , IconIndex 0
          ]
        file [] "cardano-node.exe"
        file [Recursive] "dlls\\"
        file [] "..\\log-config-prod.yaml"
        file [Recursive] "C:\\daedalus\\release\\win32-x64\\Daedalus-win32-x64\\"
        writeUninstaller "uninstall.exe"

    uninstall $ do
      delete [Recursive] "$INSTDIR"
      rmdir [] "$INSTDIR"
      delete [] "$DESKTOP\\Daedalus.lnk"
      -- Note: we leave user data alone
   
main :: IO ()
main = do
  echo "Writing daedalus.nsi"
  writeNSIS
  echo "Generating NSIS installer daedalus-win64-installer.exe"
  procs "C:\\Program Files (x86)\\NSIS\\makensis" ["daedalus.nsi"] mempty
