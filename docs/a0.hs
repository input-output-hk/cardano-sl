import Control.Monad
import Text.Printf

data Config = Config
    { cfgK   :: Int
    , cfgLam :: Double
    , cfgC   :: Double
    , cfgR   :: Double
    , cfgA0  :: Double
    } deriving Show

minS :: Config -> Double
minS cfg =
    let k   = fromIntegral (cfgK cfg)
        lam = cfgLam cfg
        c   = cfgC cfg
        r   = cfgR cfg
        a0  = cfgA0 cfg
    in  k/2 * (lam - c*(1-r)*(1+1/a0))

table :: Double -> Double -> Double -> [Double] -> IO ()
table lam c r a0s = do
    putStrLn "    \\begin{minipage}[t]{0.32\\textwidth}"
    putStrLn "        \\footnotesize"
    printf "        \\begin{flushleft}$\\tilde{\\lambda}=%f$, $\\tilde{c}=%f$, $r=%f$\\end{flushleft}\n" lam c r
    putStrLn "        \\begin{tabular}[t]{rr}"
    putStrLn "            $a_0$ & $S$ \\\\"
    putStrLn "            \\hline"
    forM_ a0s $ \a0 -> do
        let cfg = Config 100 lam c r a0
            s   = minS cfg
        printf "            %6.3f & %6.4f \\\\\n" a0 (max 0 s)
    putStrLn "        \\end{tabular}"
    putStrLn "    \\end{minipage}"

tables :: IO ()
tables = do
    bm
    table 0.01 0.001 0.9 [0.01, 0.02 .. 0.1]
    table 0.01 0.005 0.9 [0.05, 0.1 .. 0.5]
    table 0.01 0.010 0.9 [0.05, 0.1 .. 0.5]
    em
    bm
    table 0.005 0.001 0.9 [0.01, 0.02 .. 0.1]
    table 0.005 0.005 0.9 [0.05, 0.1 .. 0.5]
    table 0.005 0.010 0.9 [0.1, 0.2 .. 1]
    em
    table 0.001 0.001 0.9 [0.1, 0.2 .. 1]
    table 0.001 0.005 0.9 [0.5, 1 .. 5]
    table 0.001 0.010 0.9 [0.1, 0.2 .. 1]
    em
    bm
    table 0.01 0.001 0.5 [0.05, 0.1 .. 0.5]
    table 0.01 0.002 0.5 [0.05, 0.1 .. 0.5]
    table 0.01 0.003 0.5 [0.1, 0.2 .. 1]
    em
    bm
    table 0.005 0.001 0.5 [0.05, 0.1 .. 0.5]
    table 0.005 0.002 0.5 [0.1, 0.2 .. 1]
    table 0.005 0.003 0.5 [0.2, 0.4 .. 2]
    em
    table 0.001 0.001 0.5 [0.5, 1 .. 5]
    table 0.001 0.002 0.5 [5, 10 .. 50]
    table 0.001 0.003 0.5 [5, 10 .. 50]
    em
  where
    bm, em :: IO ()
    bm = putStrLn "\\begin{minipage}[t]{\\textwidth}"
    em = putStrLn "\\end{minipage}\n"

