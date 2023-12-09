{-# LANGUAGE LambdaCase #-}

module Day3 (main) where

import           Control.Arrow ((&&&))
import           Data.Char (isDigit, ord)
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = interact (show . (puzzle1 &&& puzzle2) . parseSchematic . lines)

puzzle1 :: Schematic -> Int
puzzle1 schematic =
    sum [ n | Part _ _ n _ <- parts schematic ]

puzzle2 :: Schematic -> Int
puzzle2 schematic =
    sum $ map starSum (Set.toList stars)
  where
    Schematic _ stars _ = schematic

    ps = parts schematic

    starSum star =
        case filter (entoothed star) ps of
            [Part _ _ n1 _, Part _ _ n2 _] -> n1 * n2

            _ -> 0

    entoothed star (Part (RowNumber r) left _ right) =
        flip any (interval left right) $ \(ColNumber c) ->
            (== star) `any` neighbors (Coord r c)

parts :: Schematic -> [Part]
parts (Schematic symbols _stars digits) =
  [ Part (RowNumber r) left number right
  | (RowNumber r, Candidate left number right) <- candidates
  , or
      [ (`Set.member` symbols) `any` neighbors (Coord r c)
      | ColNumber c <- interval left right
      ]
  ]
  where
    candidates =
        concat
          [ map ((,) r) $ pluck Nothing row
          | (r, row) <- Map.toList digits
          ]

pluck :: Maybe Candidate -> Map.Map ColNumber Int -> [Candidate]
pluck =
    \acc -> go acc . Map.toAscList
  where
    go acc = \case
        []        -> case acc of
            Nothing   -> []
            Just cand -> [cand]
        (c, d):xs -> case acc of
            Just (Candidate l n r) | touching r c ->
                 go (Just $ Candidate l (10 * n + d) c) xs

            _ -> maybe id (:) acc $ go (Just $ Candidate c d  c) xs

data Candidate = Candidate ColNumber Int ColNumber
  deriving (Show)

data Part = Part RowNumber ColNumber Int ColNumber
  deriving (Show)

-----

data Schematic = Schematic (Set.Set Coord) (Set.Set Coord) (Map.Map RowNumber (Map.Map ColNumber Int))
  deriving (Show)

instance Monoid Schematic where mempty = Schematic mempty mempty mempty

instance Semigroup Schematic where
    Schematic x1 x2 x3 <> Schematic y1 y2 y3 =
        Schematic
            (Set.union x1 y1)
            (Set.union x2 y2)
            (   Map.unionWithKey
                    (\(RowNumber r) -> Map.unionWithKey (\(ColNumber c) -> collision (Coord r c)))
                    x3
                    y3
            )
      where
        collision :: (Show k, Show v) => k -> v -> v -> v
        collision k v1 v2 = error $ "Collision! " <> show (k, v1, v2)

newtype RowNumber = RowNumber Int
  deriving (Eq, Ord, Show)

newtype ColNumber = ColNumber Int
  deriving (Eq, Ord, Show)

data Coord = Coord !Int !Int
  deriving (Eq, Ord, Show)

neighbors :: Coord -> [Coord]
neighbors (Coord r c) =
  [ Coord (r + y) (c + x)
  | x <- [-1,0,1]
  , y <- [-1,0,1]
  , (x, y) /= (0, 0)
  ]

touching :: ColNumber -> ColNumber -> Bool
touching (ColNumber x) (ColNumber y) = 1 == abs (x - y)

interval :: ColNumber -> ColNumber -> [ColNumber]
interval (ColNumber x) (ColNumber y) = map ColNumber [x .. y]

parseSchematic :: [String] -> Schematic
parseSchematic =
    foldMap mconcat . zipWith row [0 ..]
  where
    row r   = zipWith (col r) [0..]
    col r c = \case
        '.' -> mempty
        d | isDigit d -> Schematic mempty mempty (Map.singleton (RowNumber r) (Map.singleton (ColNumber c) (ord d - ord '0')))
        '*'           -> Schematic (Set.singleton (Coord r c)) (Set.singleton (Coord r c)) mempty
        _             -> Schematic (Set.singleton (Coord r c)) mempty mempty
