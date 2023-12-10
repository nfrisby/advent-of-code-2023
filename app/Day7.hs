{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Day7 (main) where

import           Control.Arrow ((&&&))
import           Data.List (sort, sortOn)
import qualified Data.Map as Map
import           Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

main :: IO ()
main = interact (show . (puzzle1 &&& puzzle2) . parse)

data Hand a = Hand a a a a a
  deriving (Eq, Functor, Ord, Show)

data HandType = HighCard | One2 | Two2 | One3 | FullHouse | One4 | One5
  deriving (Eq, Ord)

puzzle1 :: [(Hand Char, Int)] -> Int
puzzle1 =
    sum
  . zipWith (\handRank (_h, bid) -> bid * handRank) [1 ..]
  . sortOn (\(h, _bid) -> (classify h, fmap value h))

classify :: Hand Char -> HandType
classify hand =
    case mbJ of
        Nothing -> basic
        Just n  -> joker n
  where
    Hand c1 c2 c3 c4 c5 = hand

    mJ = Map.empty `f` c1 `f` c2 `f` c3 `f` c4 `f` c5

    (mbJ, m) = Map.updateLookupWithKey (\_ _ -> Nothing) 'J' mJ

    f x y = Map.insertWith (+) y (1 :: Int) x

    basic = case sort (Map.elems m) of
        [1,1,1,1,1] -> HighCard
        [1,1,1,2]   -> One2
        [1,2,2]     -> Two2
        [1,1,3]     -> One3
        [2, 3]      -> FullHouse
        [1, 4]      -> One4
        [5]         -> One5

        [1,1,1,1] -> One2
        [1,1,2]   -> One2

        o           -> error $ "impossible! " <> show (hand, o)

    joker n = case sort (Map.elems m) of
        []        -> One5
        [_]       -> One5
        [2,2]     -> FullHouse
        [_,_]     -> One4
        [_,_,_]   -> One3
        [_,_,_,_] -> One2

        o           -> error $ "impossible! " <> show (hand, n, o)

value :: Char -> Int
value = \case
    'J' -> 1
    '2' -> 2
    '3' -> 3
    '4' -> 4
    '5' -> 5
    '6' -> 6
    '7' -> 7
    '8' -> 8
    '9' -> 9
    'T' -> 10
--    'J' -> 11
    'Q' -> 12
    'K' -> 13
    'A' -> 14
    o   -> error $ "impossible! " <> show o

-----

type P = P.Parsec Void String

parse :: String -> [(Hand Char, Int)]
parse =
    \s -> case P.runParser (hands <* P.eof) "stdin" s of
        Left e  -> error $ P.errorBundlePretty e
        Right g -> g
  where
    hands :: P [(Hand Char, Int)]
    hands = P.many hand

    hand :: P (Hand Char, Int)
    hand = do
        c1 <- P.anySingle
        c2 <- P.anySingle
        c3 <- P.anySingle
        c4 <- P.anySingle
        c5 <- P.anySingle
        P.space
        bid <- L.decimal
        P.space
        pure (Hand c1 c2 c3 c4 c5, bid)

-----

puzzle2 :: [(Hand Char, Int)] -> Int
puzzle2 = puzzle1
