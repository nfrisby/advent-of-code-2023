{-# LANGUAGE LambdaCase #-}

module Day6 (main) where

import           Control.Arrow ((&&&))
import           Control.Monad (when)
import           Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

main :: IO ()
main = interact (show . (puzzle1 &&& puzzle2) . parse)

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

-----

puzzle2 :: [Race] -> Int
puzzle2 = smart . fixup

fixup :: [Race] -> Race
fixup = \case
    []         -> error "impossible!"
    race:races -> go race races
  where
    go (Race t d) = \case
        []                 -> Race t d
        Race t2 d2 : races ->
            let tpower = length (show t2)
                dpower = length (show d2)

                race' =
                    Race
                        ((10 ^ tpower) * t + t2)
                        ((10 ^ dpower) * d + d2)

            in go race' races

smart :: Race -> Int
smart (Race time record) =
    count' plusRoot `max` count' minusRoot
  where
    count' x =
        length
      $ filter ((> record) . time2Distance time)
      $ [floor (x :: Double) .. time ]

    -- distance hold = time * hold - hold * hold
    --
    -- distance = record is a quadratic:
    --
    -- a = -1; b = time; c = -record
    plusRoot  = (fromIntegral time - sqrt (fromIntegral  $ time*time - 4 * record)) / 2
    minusRoot = (fromIntegral time + sqrt (fromIntegral  $ time*time - 4 * record)) / 2
