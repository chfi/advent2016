{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}

module Main where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

data Dir = U | R | D | L deriving (Eq, Show)

parseDir :: Char -> Maybe Dir
parseDir c = case c of
  'U' -> Just U
  'D' -> Just D
  'L' -> Just L
  'R' -> Just R
  _ -> Nothing

data Keypad a = Keypad Int Int [[Maybe a]] deriving (Eq, Show)

keypadA :: Keypad Int
keypadA = Keypad 3 3 ((fmap . fmap) Just [[1,2,3],[4,5,6],[7,8,9]])

keypad :: Keypad Char
keypad = Keypad 5 5 [ [Nothing,  Nothing,  Just '1', Nothing,  Nothing]
                    , [Nothing,  Just '2', Just '3', Just '4', Nothing]
                    , [Just '5', Just '6', Just '7', Just '8', Just '9']
                    , [Nothing,  Just 'A', Just 'B', Just 'C', Nothing]
                    , [Nothing,  Nothing,  Just 'D', Nothing,  Nothing]
                    ]

type Point = (Int, Int)

movePoint :: Point -> Dir -> Point
movePoint (x,y) d = case d of
  U -> (x,y-1)
  D -> (x,y+1)
  L -> (x-1,y)
  R -> (x+1,y)

isLegal :: forall a. Keypad a -> Point -> Bool
isLegal k p = case fetchKeypad k p of
  Nothing -> False
  Just _  -> True

fetchKeypad :: forall a. Keypad a -> Point -> Maybe a
fetchKeypad (Keypad r c rows) (x,y)
  | x < 0 || x >= c || y < 0 || y >= r = Nothing
  | otherwise = (rows !! y) !! x

moveSafe :: forall a. Keypad a -> Point -> Dir -> Point
moveSafe k p d = if isLegal k p' then p' else p
  where p' = movePoint p d

parseInput :: Text -> [[Dir]]
parseInput t = fmap (mapMaybe parseDir . T.unpack) $ T.lines t

runSingle :: forall a. Keypad a -> Point -> [Dir] -> Point
runSingle k p ds = foldl (\p' d -> moveSafe k p' d) p ds

runMany :: forall a. Keypad a -> Point -> [[Dir]] -> [Point]
runMany k p rows = tail $ reverse $
  foldl (\(p:ps) row -> [runSingle k p row] ++ (p:ps)) [p] rows

main :: IO ()
main = do
  contents <- T.IO.readFile "day2input.txt"
  print $ length $ T.lines contents
  let dirs = parseInput contents
      result = runMany keypad (1, 1) dirs
  mapM_ (print . fetchKeypad keypad) result
