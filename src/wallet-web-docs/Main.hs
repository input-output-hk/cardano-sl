import           Universum

import           Pos.Wallet.Web (walletDocsText, walletTableDocsText)

main :: IO ()
main = do
  writeFile mdFp walletDocsText >> putStrLn ("See " <> mdFp)
  writeFile mdTblFp walletTableDocsText >> putStrLn ("See " <> mdTblFp)
    where
      mdFp = "docs/wallet-web-api.md"
      mdTblFp = "docs/wallet-table-web-api.md"
