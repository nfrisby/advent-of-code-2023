module Day6 (main) where

import           Control.Monad (when)
import           Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

main :: IO ()
main = interact (show . puzzle1 . parse)

data Race = Race Int Int
  deriving (Show)

time2Distance :: Int -> Int -> Int
time2Distance time hold =
    distance
  where
    traveling = time - hold

    speed = hold

    distance = traveling * speed

puzzle1 :: [Race] -> Int
puzzle1 = product . map count

count :: Race -> Int
count (Race time record) =
    length $ filter p [0 .. time]
  where
    p hold = time2Distance time hold > record

-----

type P = P.Parsec Void String

parse :: String -> [Race]
parse =
    \s -> case P.runParser (races <* P.eof) "stdin" s of
        Left e  -> error $ P.errorBundlePretty e
        Right g -> g
  where
    races :: P [Race]
    races = do
        _ <- P.string "Time:"; P.space
        times <- P.many (L.decimal <* P.space)
        _ <- P.string "Distance:"; P.space
        distances <- P.many (L.decimal <* P.space)
        when (length times /= length distances) $ fail "impossible!"
        pure $ zipWith Race times distances
