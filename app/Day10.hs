{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ParallelListComp #-}

module Day10 (main) where

import           Control.Arrow ((&&&))
import           Control.Monad (guard)
import qualified Data.List as L
import qualified Data.Set as Set
import           Day3 (Coord (..), neighbors)

main :: IO ()
main = interact (show . (puzzle1 &&& puzzle2) . parse)

puzzle1 :: (Coord, Grid) -> Int
puzzle1 (s, grid) =
    let (a, b, _grid') = first2 (s, grid) in go 1 a b
  where
    go !acc a b =
        if tip a == tip b then acc else
        go (acc + 1) (step grid a) (step grid b)

first2 :: (Coord, Grid) -> (Flow, Flow, Grid)
first2 (s, grid) =
    case filter p (neighbors s) of
        [x, y] -> (Flow s x, Flow s y, set s (decodeStart s (x, y)) grid)
        o      -> error $ "impossible! " <> show o
  where
    p x = valid grid x && s `elem2` connections grid x

set :: Coord -> Char -> Grid -> Grid
set (Coord r c) col' (Grid rows) =
    Grid $ ltrow ++ row' : tail gerow
  where
    -- fst of L.splitAt n has length n
    (ltrow, gerow) = L.splitAt r rows

    (ltcol, gecol) = L.splitAt c (head gerow)

    row' = ltcol ++ col' : tail gecol

-----

newtype Grid = Grid [[Char]]
  deriving (Show)

data Flow = Flow Coord Coord
  deriving (Show)

tip :: Flow -> Coord
tip (Flow _ x) = x

valid :: Grid -> Coord -> Bool
valid (Grid rows) (Coord r c) =
    0 <= r && r < eubR
 && 0 <= c && c < eubC
  where
    eubR = length rows
    eubC = length (head rows)

lu :: Grid -> Coord -> Char
lu (Grid rows) (Coord r c) = rows !! r !! c

elem2 :: Eq a => a -> Maybe (a, a) -> Bool
elem2 needle = \case
    Nothing     -> False
    Just (x, y) -> x == needle || y == needle

connections :: Grid -> Coord -> Maybe (Coord, Coord)
connections grid x = connections' x (lu grid x)

connections' :: Coord -> Char -> Maybe (Coord, Coord)
connections' x content = case content of
    '|' -> just north south
    '-' -> just east  west
    'L' -> just north east
    'J' -> just north west
    '7' -> just south west
    'F' -> just south east
    _   -> Nothing
  where
    Coord r c = x

    north = Coord (r - 1) c
    east  = Coord r       (c + 1)
    south = Coord (r + 1) c
    west  = Coord r       (c - 1)

    just a b = Just (a, b)

step :: Grid -> Flow -> Flow
step grid (Flow prev here) =
    Flow here x
  where
    x = case connections grid here of
        Nothing     -> error "impossible!"
        Just (a, b) -> if a == prev then b else a

-----

parse :: String -> (Coord, Grid)
parse s =
    (Coord r c, Grid rows)
  where
    rows = lines s

    r = maybe (err "row") id $ L.findIndex ('S' `elem`) rows

    c = maybe (err "col") id $ L.findIndex ('S' ==) (rows !! r)

    err e = error $ "impossible! " <> e <> "\n" <> s

-----

-- I vaguely recalled the "winding number" had something to do with
-- this. Starting there, I soon found myself at
-- https://en.wikipedia.org/wiki/Point_in_polygon, which seems very
-- relevant. It should work even though all of our polygon's internal
-- angles are 90 degrees, I assume.
--
-- However, the ray casting idea on that page seems simpler: we can
-- just travel down each column, tracking whether how many times we've
-- crossed the loop. It's slightly tricky since our loop has girth as
-- opposed to the infinitesimal side of polygon. But it's not too bad
-- to overcome.

-- Algorithm
--
-- Take this example
--
--   ...........
--   .S-------7.
--   .|F-----7|.
--   .||.....||.
--   .||.....||.
--   .|L-7.F-J|.
--   .|..|.|..|.
--   .L--J.L--J.
--   ...........
--
-- And specifically this row
--
--   .|L-7.F-J|.
--
-- That should be mapped to the following in 'horizontal'.
--
--   FTTTTFTTTTF
--   01222344456

puzzle2 :: (Coord, Grid) -> Int
puzzle2 (s, grid) =
    length
  $ filter id
      [ not flag && odd cross
      | State _mbStart cross <- concat verticalStates
      | flag                 <- concat inTheLoop
      ]
  where
    (flow1, flow2, grid') = first2 (s, grid)

    theLoop = Set.insert s $ Set.fromList $ go1 flow1 flow2

    go1 a b =
        if tip a == tip b then [tip a] else
            tip a
          : tip b
          : go1 (step grid a) (step grid b)

    inTheLoop =
        let Grid rows = grid
        in
          [   [ Coord r c `Set.member` theLoop
              | _col <- row | c <- [0..]
              ]
          | row <- rows | r <- [0..]
          ]

    verticalStates =
        let Grid rows = grid' in go2 Coord (repeat top) 0 rows

    top = State Nothing 0

    go2 coord acc r = \case
        []       -> []
        row:rows ->
            let acc' =
                    [ enterFromAbove theLoop state content (coord r c)
                    | state <- acc | content <- row | c <- [0..]
                    ]
            in acc' : go2 coord acc' (r + 1) rows

enterFromAbove ::
    Ord coord
 => Set.Set coord -> State -> Char -> coord -> State
enterFromAbove theLoop (State mbStart cross) content here =
    case (mbStart, content) of
        (Nothing , _  ) -> State mbStart' cross

        (Just '-', _  ) -> State mbStart' (cross + 1)   -- just an ollie

        (Just{}  , '|') -> State mbStart cross   -- still grinding

        (Just '7', 'J') -> dismountNoCross
        (Just '7', 'L') -> dismountAcross

        (Just 'F', 'J') -> dismountAcross
        (Just 'F', 'L') -> dismountNoCross

        o -> error $ "impossible! " <> show o
  where
    mbStart' = do guard $ here `Set.member` theLoop; Just content

    -- We moved along a section of the pipe, but we never
    -- crossed its boundary. (Like the typical way a skater
    -- grinds a rail.)
    dismountNoCross = State Nothing cross

    -- We moved along a section of the pipe, but and eventuall
    -- did cross its boundary. (Like the atypical way a skater
    -- grinds a rail: entering the side they did not mount.)
    dismountAcross = State Nothing (cross + 1)

-- | Which pieces is 'S' representing?
decodeStart :: Coord -> (Coord, Coord) -> Char
decodeStart (Coord r c) (Coord r1 c1, Coord r2 c2)
  | c1 == c2 = '|'
  | r1 == r2 = '-'

  | up  , left  = 'J'
  | up  , right = 'L'
  | down, left  = '7'
  | down, right = 'F'

  | otherwise = error $ "impossible! " <> show (r, c, r1, c1, r2, c2)

  where
    up   = r1 < r || r2 < r
    down = r < r1 || r < r2

    left  = c1 < c || c2 < c
    right = c < c1 || c < c2

-- | The state after entering a coordinate from above
--
-- If we're in a loop, 'Just' the piece we first entered.
--
-- The number of times we have crossed the loop.
data State = State (Maybe Char) Int
