import           Universum

import           Pos.Wallet.Web (walletDocsText)

main :: IO ()
main = do
  writeFile fp walletDocsText >> putStrLn ("See " <> fp)
    where
      fp = "docs/wallet-web-api.md"
