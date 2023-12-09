{-# LANGUAGE LambdaCase #-}

module Day5 (main) where

import           Control.Arrow ((&&&))
import qualified Data.Map as Map
import           Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

main :: IO ()
main = interact (show . puzzle1 . parse)

puzzle1 :: Almanac -> Int
puzzle1 almanac =
    case map (seedToLocation almanac) seeds of
        []   -> negate 1
        x:xs -> foldl min x xs
  where
    Almanac seeds _ _ _ _ _ _ _ = almanac

-----

type Map = Map.Map Int Ival

-- seed soil fert water light temp humid loc
data Almanac = Almanac [Int] Map  Map Map Map Map Map Map
  deriving (Show)

-- start and size
data Ival = Ival !Int !Int
  deriving (Eq, Ord, Show)

type P = P.Parsec Void String

parse :: String -> Almanac
parse =
    \s -> case P.runParser (almanac <* P.eof) "stdin" s of
        Left e  -> error $ P.errorBundlePretty e
        Right g -> g
  where
    almanac :: P Almanac
    almanac = do
        _ <- P.string "seeds:"; P.space
        seeds <- P.many (L.decimal <* P.space)
        _ <- P.string "seed-to-soil map:"; P.space
        x1 <- ivals
        _ <- P.string "soil-to-fertilizer map:"; P.space
        x2 <- ivals
        _ <- P.string "fertilizer-to-water map:"; P.space
        x3 <- ivals
        _ <- P.string "water-to-light map:"; P.space
        x4 <- ivals
        _ <- P.string "light-to-temperature map:"; P.space
        x5 <- ivals
        _ <- P.string "temperature-to-humidity map:"; P.space
        x6 <- ivals
        _ <- P.string "humidity-to-location map:"; P.space
        x7 <- ivals
        pure $ Almanac seeds x1 x2 x3 x4 x5 x6 x7

    ivals :: P Map
    ivals = Map.fromList <$> P.many (ival <* P.space)

    ival :: P (Int, Ival)
    ival = do
        dest <- L.decimal <* P.space
        src  <- L.decimal <* P.space
        n    <- L.decimal
        pure (src, Ival dest n)

-----

seedToLocation :: Almanac -> Int -> Int
seedToLocation almanac seed =
    apply x7
  $ apply x6
  $ apply x5
  $ apply x4
  $ apply x3
  $ apply x2
  $ apply x1
  $ seed
  where
    Almanac _ x1 x2 x3 x4 x5 x6 x7 = almanac

apply :: Map -> Int -> Int
apply m x =
    case eq of
        Just (Ival dest _) -> dest
        Nothing            -> case Map.maxViewWithKey lt of
            Nothing                     -> x
            Just ((src, Ival dest n), _) ->
                if src + n < x then x else dest + (x - src)
  where
    (lt, eq, _gt) = Map.splitLookup x m
