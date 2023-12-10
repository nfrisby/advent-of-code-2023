{-# LANGUAGE LambdaCase #-}

module Main (main) where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import           System.Environment (getArgs)
import           Text.Read (readMaybe)

main :: IO ()
main = getArgs >>= \case
    [s]
      | Just m <- readMaybe s >>= doDay -> m
    _ -> fail "$1 must be an integer for an implemented day"

doDay :: Int -> Maybe (IO ())
doDay = \case
    1  -> Just Day1.main
    2  -> Just Day2.main
    3  -> Just Day3.main
    4  -> Just Day4.main
    5  -> Just Day5.main
    6  -> Just Day6.main
    7  -> Just Day7.main
    8  -> Just Day8.main
    9  -> Just Day9.main
    10 -> Just Day10.main
    _  -> Nothing
