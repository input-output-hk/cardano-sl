{-# LANGUAGE CPP #-}

import           Universum

#if defined(WITH_WALLET) && defined(WITH_WEB)
import           Pos.Wallet.Web (walletDocsText)


main :: IO ()
main = writeFile fp walletDocsText >> putStrLn ("See " <> fp)
  where
    fp = "wallet-web-api.md"

#else
main :: IO ()
main = panic "Wallet or web is disabled!"
#endif
