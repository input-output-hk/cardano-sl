module Main (main) where

import Universum
import Pos.Explorer.Web.Doc (walletDocsText, walletTableDocsText)

main :: IO ()
main = do
  writeFile mdFp walletDocsText >> putStrLn ("See " <> mdFp)
  writeFile mdTblFp walletTableDocsText >> putStrLn ("See " <> mdTblFp)
    where
      mdFp = "docs/cardano-explorer-web-api.md"
      mdTblFp = "docs/cardano-explorer-table-web-api.md"
