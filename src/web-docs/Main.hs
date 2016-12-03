{-# LANGUAGE CPP #-}

import           Universum

#ifdef WITH_WEB
import           Pos.Web   (gtDocsText)


main :: IO ()
main = writeFile fp gtDocsText >> putStrLn ("See " <> fp)
  where
    fp = "web-api.md"
#else
main :: IO ()
main = panic "Web is disabled!"
#endif
