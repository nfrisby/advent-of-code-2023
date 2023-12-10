{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day5 (main) where

import           Control.Arrow ((&&&))
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Void (Void)
import qualified Test.QuickCheck as QC
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

main :: IO ()
main = do
    QC.quickCheckWith QC.stdArgs{QC.maxSuccess=10000} prop_applyIval
    interact (show . (puzzle1 &&& puzzle2) . parse)

puzzle1 :: Almanac -> Int
puzzle1 almanac =
    case map (seedToLocation almanac) (concatMap whoops seeds) of
        []   -> negate 1
        x:xs -> foldl min x xs
  where
    Almanac seeds _ _ _ _ _ _ _ = almanac

    whoops (Ival lo n) = [lo, n]

puzzle2 :: Almanac -> Int
puzzle2 almanac =
    case seeds `o` x1 `o` x2 `o` x3 `o` x4 `o` x5 `o` x6 `o` x7 of
       []   -> negate 1
       x:xs -> foldl min (start x) (map start xs)
  where
    Almanac seeds x1 x2 x3 x4 x5 x6 x7 = almanac

    start (Ival lo _) = lo

    ivals `o` m = concatMap (\ival -> applyIval id ival m) ivals

-----

type Map' a b = Map.Map a (Ival' b)

toAscList :: Map' a b -> [IIval a b]
toAscList =
    map f . Map.toAscList
  where
    f (src, Ival dest n) = IIval src dest n

type Map = Map' Int Int

-- seed soil fert water light temp humid loc
data Almanac = Almanac [Ival] Map Map Map Map Map Map Map
  deriving (Show)

-----

type Ival = Ival' Int

-- start and size
data Ival' b = Ival !b !Int
  deriving (Eq, Ord, Show)

ivalValues :: Enum a => Ival' a -> [a]
ivalValues (Ival x n) = [x .. x .+ (n - 1)]

-- both starts and size
data IIval a b = IIval !a !b !Int
  deriving (Eq, Ord, Show)

-- | An interval being overridden by new intervals given one-by-one
newtype State a = State (Ival' a)
data Step a b = Lesser | Progress (Maybe (Ival' b)) (Ival' b) (Maybe (State a)) | Finished (Ival' b)

(.+) :: Enum a => a -> Int -> a
x .+ n = toEnum $ fromEnum x + n

(.-) :: Enum a => a -> a -> Int
x .- y = fromEnum x - fromEnum y

-- | Update an 'Ival' of interest by a given 'IIVal', default to
-- identity for any skipped @a@s
--
-- The next 'IIval' used to override the resulting 'State' must be
-- apart and greater than this given 'IIval'.
ivalNextOverride :: (Ord a, Enum a, Enum b) => (a -> b) -> State a -> IIval a b -> Step a b
ivalNextOverride ab st given
  | hiGiven < next = Lesser
  | hiState < src  = Finished $ ivalNoMoreOverrides ab st
  | otherwise      =
    Progress
        (if skippedA < 1 then Nothing else Just $ Ival (ab next) skippedA)
        ((dest .+ skippedB) `Ival` (min limit k - skippedB))
        (State <$> mkIval (hiGiven .+ 1) hiState)
  where
    State (Ival next n) = st
    IIval src dest k    = given

    hiState = next .+ (n - 1)
    hiGiven = src  .+ (k - 1)

    skippedA = src .- next
    skippedB = max 0 $ next .- src

    limit = hiState .- src + 1  -- cannot progress more than this

ivalNoMoreOverrides :: (a -> b) -> State a -> Ival' b
ivalNoMoreOverrides ab (State (Ival next n)) = Ival (ab next) n

mkIval :: (Ord a, Enum a) => a -> a -> Maybe (Ival' a)
mkIval lo hi = if hi < lo then Nothing else Just $ Ival lo ((hi .- lo) + 1)

-----

type P = P.Parsec Void String

parse :: String -> Almanac
parse =
    \s -> case P.runParser (almanac <* P.eof) "stdin" s of
        Left e  -> error $ P.errorBundlePretty e
        Right g -> g
  where
    almanac :: P Almanac
    almanac = do
        _ <- P.string "seeds:"; P.space
        seeds <- P.many (Ival <$> L.decimal <* P.space <*> L.decimal <* P.space)
        _ <- P.string "seed-to-soil map:"; P.space
        x1 <- ivals
        _ <- P.string "soil-to-fertilizer map:"; P.space
        x2 <- ivals
        _ <- P.string "fertilizer-to-water map:"; P.space
        x3 <- ivals
        _ <- P.string "water-to-light map:"; P.space
        x4 <- ivals
        _ <- P.string "light-to-temperature map:"; P.space
        x5 <- ivals
        _ <- P.string "temperature-to-humidity map:"; P.space
        x6 <- ivals
        _ <- P.string "humidity-to-location map:"; P.space
        x7 <- ivals
        pure $ Almanac seeds x1 x2 x3 x4 x5 x6 x7

    ivals :: P Map
    ivals = Map.fromList <$> P.many (ival <* P.space)

    ival :: P (Int, Ival)
    ival = do
        dest <- L.decimal <* P.space
        src  <- L.decimal <* P.space
        n    <- L.decimal
        pure (src, Ival dest n)

-----

seedToLocation :: Almanac -> Int -> Int
seedToLocation almanac seed =
    apply id x7
  $ apply id x6
  $ apply id x5
  $ apply id x4
  $ apply id x3
  $ apply id x2
  $ apply id x1
  $ seed
  where
    Almanac _ x1 x2 x3 x4 x5 x6 x7 = almanac

apply :: (Ord a, Enum a, Enum b) => (a -> b) -> Map' a b -> a -> b
apply ab m x =
    case eq of
        Just (Ival dest _) -> dest
        Nothing            -> case Map.maxViewWithKey lt of
            Nothing                      -> ab x
            Just ((src, Ival dest n), _) ->
                if src .+ n <= x then ab x else dest .+ (x .- src)
  where
    (lt, eq, _gt) = Map.splitLookup x m

applyIval ::
 forall a b.
     (Ord a, Enum a, Enum b)
 => (a -> b)
 -> Ival' a
 -> Map' a b
 -> [Ival' b]
applyIval ab ival m =
    go (State ival) (toAscList m)
  where
    go :: State a -> [IIval a b] -> [Ival' b]
    go st = \case
        []                 -> [ivalNoMoreOverrides ab st]
        override:overrides -> case ivalNextOverride ab st override of
            Lesser                        -> go st overrides
            Finished final                -> [final]
            Progress mbSkipped next mbSt' -> 
                maybe id (:) mbSkipped
              $ next : case mbSt' of
                    Nothing  -> []
                    Just st' -> go st' overrides

-----

newtype Z = Z Map
  deriving (Show)

instance QC.Arbitrary Ival where
    arbitrary = Ival <$> QC.choose (0, 90) <*> QC.choose (1, 9)

    shrink (Ival dest n) =
        filter valid $ map (uncurry Ival) $ QC.shrink (dest, n)
      where
        valid (Ival _ n') = 0 < n'

instance QC.Arbitrary Z where
    arbitrary = fmap Z $ flip QC.suchThat validMap $ do
        n <- QC.choose (0, 5)
        sizes <- sequence $ replicate n $ QC.choose (1, 9)
        srcs  <- go 0 sizes
        dests <- go 0 sizes >>= QC.shuffle
        pure $ Map.fromList $ zip srcs (zipWith Ival dests sizes)
      where
        go acc = \case
            []         -> pure []
            size:sizes -> do
                skip <- QC.choose (0, 20)
                xs   <- go (acc + size + skip) sizes
                pure $ acc : xs

    shrink (Z x) =
         map Z $ filter validMap $ QC.shrink x
      where

validMap :: Map -> Bool
validMap m =
    check (Map.toAscList $ Map.map (\(Ival _dest n) -> n) m)
 &&
  ( check
  $ Map.toAscList
  $ Map.fromList
  $ map (\(Ival dest n) -> (dest, n))
  $ Map.elems m
  )
 &&
    and [ src /= dest | IIval src dest _ <- toAscList m ]
  where
    check = \case
        []  -> True
        [_] -> True

        (k, n):(l, o):rest -> k + n <= l && check ((l, o):rest)

-----

prop_applyIval :: Z -> Ival -> QC.Property
prop_applyIval (Z m) ival =
    QC.counterexample "-----"
  $ QC.counterexample (show m)
  $ QC.counterexample (show ival)
  $ QC.counterexample "-----"
  $ QC.counterexample (show ivals)
  $ QC.counterexample "-----"
  $ obvious QC.=== computed
  where
    obvious = canon $ apply id m `map` ivalValues ival

    computed = canon $ concatMap ivalValues ivals

    ivals = applyIval id ival m

    canon = Set.toList . Set.fromList
