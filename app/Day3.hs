{-# LANGUAGE LambdaCase #-}

module Day3 (main) where

import           Data.Char (isDigit, ord)
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = interact (show . sum . partNumbers . parseSchematic . lines)

partNumbers :: Schematic -> [Int]
partNumbers (Schematic symbols digits) =
  [ number
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

-----

data Schematic = Schematic (Set.Set Coord) (Map.Map RowNumber (Map.Map ColNumber Int))
  deriving (Show)

instance Monoid Schematic where mempty = Schematic mempty mempty

instance Semigroup Schematic where
    Schematic x1 x2 <> Schematic y1 y2 =
        Schematic
            (Set.union x1 y1)
            (   Map.unionWithKey
                    (\(RowNumber r) -> Map.unionWithKey (\(ColNumber c) -> collision (Coord r c)))
                    x2
                    y2
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
        d | isDigit d -> Schematic mempty (Map.singleton (RowNumber r) (Map.singleton (ColNumber c) (ord d - ord '0')))
        _             -> Schematic (Set.singleton (Coord r c)) mempty
