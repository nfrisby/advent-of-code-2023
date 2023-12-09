{-# LANGUAGE LambdaCase #-}

module Day4 (main) where

import           Control.Arrow ((&&&))
import qualified Data.Set as Set
import           Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

main :: IO ()
main = interact (show . (puzzle1 &&& puzzle2) . map parse . lines)

puzzle1 :: [Card] -> Int
puzzle1 = sum . map points

-----

data Card = Card Int [Int] [Int]
  deriving (Show)

type P = P.Parsec Void String

parse :: String -> Card
parse =
    \s -> case P.runParser (card <* P.eof) "stdin" s of
        Left e  -> error $ P.errorBundlePretty e
        Right g -> g
  where
    card :: P Card
    card = do
        _ <- P.string "Card"
        P.space
        ident <- L.decimal
        _ <- P.string ":"
        P.space
        winning <- P.many $ L.decimal <* P.space
        _ <- P.string "|"
        P.space
        mine <- P.many $ L.decimal <* P.space
        pure $ Card ident winning mine

matches :: Card -> Int
matches (Card _ winning mine) =
    length $ filter (`Set.member` Set.fromList winning) mine

points :: Card -> Int
points c =
    if 0 == n then 0 else 2 ^ (n - 1)
  where
    n = matches c

-----

puzzle2 :: [Card] -> Int
puzzle2 =
    \cards -> go (length cards) cards
  where
    go n = \case
        _ | 0 == n -> 0

        []   -> 0
        c:cs ->
            let thisCard = 1
                oldCards = go (n-1) cs
                newCards = go (matches c) cs
            in
            thisCard + oldCards + newCards
