module Day9 (main) where

import           Control.Arrow ((&&&))

main :: IO ()
main = interact (show . (puzzle1 &&& puzzle2) . parse)

parse :: String -> [[Int]]
parse = map (map read . words) . lines

puzzle1 :: [[Int]] -> Int
puzzle1 = sum . map (fst . predict)

predict :: [Int] -> (Int, Int)
predict =
    go
  where
    go sq =
        if all (==0) sq
        then (0, 0)
        else ( head sq - fst (go (diff sq))
             , last sq + snd (go (diff sq))
             )

diff :: [Int] -> [Int]
diff x = zipWith (-) (tail x) (init x)

-----

puzzle2 :: [[Int]] -> Int
puzzle2 = sum . map (snd . predict)
