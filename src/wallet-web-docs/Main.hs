import           Universum

import           Pos.Wallet.Web (walletDocsText)

main :: IO ()
main = writeFile fp walletDocsText >> putStrLn ("See " <> fp)
  where
    fp = "wallet-web-api.md"
