{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Control.Concurrent (threadDelay)
import System.Posix.Unistd (usleep)
import System.Environment (getArgs)
import Network.StatsD.Datadog

main :: IO ()
main = do
    args <- getArgs
    let m :: Int
        m = read (head args)
    withDogStatsD (DogStatsSettings "127.0.0.1" 8125) $ \c -> loop c m 0

loop :: StatsClient -> Int -> Int -> IO ()
loop c m i | i == m    = pure ()
           | otherwise = do
    forM_ [1..100] $ \_ ->
        send c (metric (MetricName "i") Gauge m)
    threadDelay 1000
    loop c m (i + 1)
