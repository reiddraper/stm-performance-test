module Main where

import Control.Concurrent.STM
import Control.Monad (replicateM, replicateM_)

import Criterion.Main (Benchmark, defaultMain, bgroup, bench, nfIO)

test :: Int -> IO ()
test numItems = do
    replicateM numItems newEmptyTMVarIO >>= \vars -> atomically (Prelude.foldr orElse retry ((fmap takeTMVar vars) ++ [(return ())]))

benchmarks :: [Benchmark]
benchmarks =
    [ bench "1" $ nfIO $ test 1
    , bench "2" $ nfIO $ test 2
    , bench "4" $ nfIO $ test 4
    , bench "8" $ nfIO $ test 8
    , bench "16" $ nfIO $ test 16
    , bench "32" $ nfIO $ test 32
    , bench "64" $ nfIO $ test 64
    , bench "128" $ nfIO $ test 128
    , bench "256" $ nfIO $ test 256
    , bench "512" $ nfIO $ test 512
    , bench "1024" $ nfIO $ test 1024
    , bench "2048" $ nfIO $ test 2048
    ]

main :: IO ()
main = defaultMain [bgroup "transactions" benchmarks]
