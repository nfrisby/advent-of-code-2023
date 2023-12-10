{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Day8 (main) where

import           Control.Arrow ((&&&))
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

main :: IO ()
main = interact (show . (uncurry puzzle1 &&& uncurry puzzle2) . parse)

puzzle1 :: Map -> [Turn] -> Int
puzzle1 nodes =
    if not $ Map.member (Loc "AAA") nodes then const 0 else
    go 0 (Loc "AAA") . cycle
  where
    go acc loc turns =
        if Loc "ZZZ" == loc then acc else
        case (Map.lookup loc nodes, turns) of
            (Just (l, r), turn:turns') ->
                go (acc + 1) (case turn of L -> l; R -> r) turns'

            _ -> error $ "impossible! " <> show (loc, null turns)
            
data Turn = L | R
  deriving (Show)

newtype Loc = Loc String
  deriving (Eq, Ord, Show)

type Map = Map.Map Loc (Loc, Loc)

-----

type P = P.Parsec Void String

parse :: String -> (Map, [Turn])
parse =
    \s -> case P.runParser (instructions <* P.eof) "stdin" s of
        Left e  -> error $ P.errorBundlePretty e
        Right g -> g
  where
    instructions :: P (Map, [Turn])
    instructions = do
        turns <- P.many $ (L <$ P.string "L") P.<|> (R <$ P.string "R")
        P.space
        nodes <- Map.fromList <$> P.many node
        pure (nodes, turns)

    node :: P (Loc, (Loc, Loc))
    node = do
        here <- loc
        P.space
        _ <- P.string "="
        P.space
        _ <- P.string "("
        l <- loc
        _ <- P.string ","
        P.space
        r <- loc
        _ <- P.string ")"
        P.space
        pure (here, (l, r))
        
    loc :: P Loc
    loc = (Loc <$> P.many P.alphaNumChar) <* P.space


-----

puzzle2 :: Map -> [Turn] -> Int
puzzle2 nodes turns =
    combine
  $ map simplifyingAssumption
  $ findCycles nodes (cycle turns) `map` starts
  where
    starts = filter endsWithA $ Map.keys nodes

    simplifyingAssumption = \case
        [dist] -> dist
        o      -> error $ "impossible! " <> show o

    combine = \case
        x:xs -> foldl lcm x xs
        []   -> error "impossible!"

endsWithA :: Loc -> Bool
endsWithA (Loc s) = last s == 'A'

endsWithZ :: Loc -> Bool
endsWithZ (Loc s) = last s == 'Z'

findCycles :: Map -> [Turn] -> Loc -> [Int]
findCycles nodes =
    \turns loc -> go2 Set.empty $ go1 0 turns loc
  where
    go1 !acc turns loc =
        (if endsWithZ loc then ((loc, acc) :) else id)
      $ case (Map.lookup loc nodes, turns) of
            (Just (l, r), turn:turns') ->
                go1 (acc + 1) turns' (case turn of L -> l; R -> r)
            _                          -> error $ "impossible! " <> show loc

    go2 !acc = \case
        []               -> []
        (loc, dist):rest ->
            if Set.member loc acc then [] else
            dist : go2 (Set.insert loc acc) rest
