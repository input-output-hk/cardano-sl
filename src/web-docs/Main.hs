import           Universum

import           Pos.Web   (gtDocsText)


main :: IO ()
main = writeFile fp gtDocsText >> putStrLn ("See " <> fp)
  where
    fp = "web-api.md"
