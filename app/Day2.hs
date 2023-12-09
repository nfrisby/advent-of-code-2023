{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Day2 (main) where

import           Control.Arrow ((&&&))
import           Control.Monad (guard)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (mapMaybe)
import           Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

main :: IO ()
main = interact $ show . (part1 &&& part2) . map parse . lines

part1 :: [Game] -> Int
part1 = sum . mapMaybe possible

data Game = Game !Int (NE.NonEmpty (Pull Int))

data Pull a = Pull a a a
  deriving (Foldable, Functor)

instance Applicative Pull where
    pure x = Pull x x x

    Pull f1 f2 f3 <*> Pull x1 x2 x3 = Pull (f1 x1) (f2 x2) (f3 x3)

hypothesis :: Pull Int
hypothesis = Pull 12 13 14

possible :: Game -> Maybe Int
possible (Game ident pulls) =
    do guard (all p pulls); Just ident
  where
    p pull = and $ (<=) <$> pull <*> hypothesis

-----

type P = P.Parsec Void String

parse :: String -> Game
parse =
    \s -> case P.runParser game "stdin" s of
        Left e  -> error $ show e
        Right g -> g
  where
    game :: P Game
    game = do
        _ <- P.string "Game"
        P.space
        ident <- L.decimal
        _ <- P.string ":"
        P.space
        x <- pull
        _ <- P.string ";"; P.space
        xs <- pull `P.sepBy` (do _ <- P.string ";"; P.space)
        pure $ Game ident $ x NE.:| xs

    pull :: P (Pull Int)
    pull = do
        xs <- pull1 `P.sepBy` (do _ <- P.string ","; P.space)
        pure $ foldr (\x y -> max <$> x <*> y) (pure 0) xs

    pull1 :: P (Pull Int)
    pull1 = do
        count <- L.decimal
        P.space
        P.many P.letterChar >>= \case
            "red"   -> pure $ Pull count 0 0
            "green" -> pure $ Pull 0 count 0
            "blue"  -> pure $ Pull 0 0 count
            o       -> fail $ "bad color: " <> o

-----

part2 :: [Game] -> Int
part2 = sum . map gamePower

gamePower :: Game -> Int
gamePower (Game _ident pulls) =
    power
  $ let p NE.:| ps = pulls
    in foldr (\x y -> max <$> x <*> y) p ps

power :: Num a => Pull a -> a
power (Pull a b c) = a * b * c
