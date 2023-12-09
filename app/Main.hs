{-# LANGUAGE LambdaCase #-}

module Main (main) where

import qualified Day1
import           System.Environment (getArgs)
import           Text.Read (readMaybe)

main :: IO ()
main = getArgs >>= \case
    [s]
      | Just m <- readMaybe s >>= doDay -> m
    _ -> fail "$1 must be an integer for an implemented day"

doDay :: Int -> Maybe (IO ())
doDay = \case
    1 -> Just Day1.main
    _ -> Nothing
