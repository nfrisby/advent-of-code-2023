{-# LANGUAGE LambdaCase #-}

module Day4 (main) where

import qualified Data.Set as Set
import           Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

main :: IO ()
main = interact (show . sum . map (points . parse) . lines)

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

points :: Card -> Int
points (Card _ winning mine) =
    if 0 == wins then 0 else 2 ^ (wins - 1)
  where
    wins = length $ filter (`Set.member` Set.fromList winning) mine
