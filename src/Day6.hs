{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List

import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

parseInput :: Text -> [Text]
parseInput = T.transpose . T.lines

occurrences :: (Ord a) => [a] -> Map a Int
occurrences = foldr (\a m -> Map.insertWith (\_ o -> (o + 1)) a 1 m) Map.empty

decodeLine :: (Int -> Int -> Ordering) -> Text -> Char
decodeLine f = fst . maximumBy (\(_,i) (_,j) -> f i j) . Map.toList . occurrences . T.unpack

main = do
  contents <- T.IO.readFile "day6input.txt"
  print $ fmap (decodeLine (flip compare)) $ parseInput contents
  print $ fmap (decodeLine compare) $ parseInput contents
