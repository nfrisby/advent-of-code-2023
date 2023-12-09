{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Day1 (main) where

import           Control.Arrow ((&&&))
import           Data.List (isPrefixOf)
import qualified Data.Semigroup as Semi

main :: IO ()
main = interact $ show . (part1 &&& part2) . lines

part1 :: [String] -> Int
part1 = sum . map decode

decode :: String -> Int
decode s =
    paste s
        ( f Semi.getFirst Semi.First
        , f Semi.getLast  Semi.Last
        )
  where
    f :: Semigroup w => (w -> Int) -> (Int -> w) -> Maybe Int
    f x y = fmap x $ pluck (Just . y) s
  
paste :: String -> (Maybe Int, Maybe Int) -> Int
paste s = \case
    (Just x, Just y) -> 10 * x + y
    _ -> error $ "bad: " <> s

pluck :: Monoid w => (Int -> w) -> String -> w
pluck inj = foldMap $ \case
    '0' -> inj 0
    '1' -> inj 1
    '2' -> inj 2
    '3' -> inj 3
    '4' -> inj 4
    '5' -> inj 5
    '6' -> inj 6
    '7' -> inj 7
    '8' -> inj 8
    '9' -> inj 9
    _   -> mempty

-----

part2 :: [String] -> Int
part2 = sum . map (decode . bake)

bake :: String -> String
bake =
    id
  . expand ("zero" , '0')
  . expand ("one"  , '1')
  . expand ("two"  , '2')
  . expand ("three", '3')
  . expand ("four" , '4')
  . expand ("five" , '5')
  . expand ("six"  , '6')
  . expand ("seven", '7')
  . expand ("eight", '8')
  . expand ("nine" , '9')

expand :: (String, Char) -> String -> String
expand (name, digit) =
    go
  where
    go s = case s of
      []   -> []
      c:cs ->
          (\x -> if name `isPrefixOf` s then name <> [digit] <> x else x)
        $ c : go cs
