{-# LANGUAGE LambdaCase #-}

module Day1 (main) where

import qualified Data.Semigroup as Semi

main :: IO ()
main = interact $ show . sum . map decode . lines

decode :: String -> Int
decode s =
    paste s
        ( fmap Semi.getFirst $ pluck (Just . Semi.First) s
        , fmap Semi.getLast  $ pluck (Just . Semi.Last ) s
        )
  
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
