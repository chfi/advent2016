{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative

import Data.List
import Data.Either (rights)

import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T.IO

import           Data.Attoparsec.Text hiding (take)

screenWidth, screenHeight :: Int
screenWidth = 50
screenHeight = 6

data Orientation = Row | Column deriving (Eq, Show)

data Inst = Rect Int Int
          | Rotate Orientation Int Int
          deriving (Eq, Show)

type Screen = [[Bool]]

blankScreen :: Int -> Int -> Screen
blankScreen rs cs = take rs $ repeat $ take cs $ repeat False

shiftList :: [a] -> Int -> [a]
shiftList l i = take (length l) $ drop (length l - i) $ cycle l

rotateRow :: Int -> Int -> Screen -> Screen
rotateRow r i s = s'
  where s' = a ++ [shiftList row i] ++ b
        (a,row:b) = splitAt r s

rotateColumn :: Int -> Int -> Screen -> Screen
rotateColumn r i s = transpose $ rotateRow r i $ transpose s

fillLine :: Int -> [Bool] -> [Bool]
fillLine i r = take i (repeat True) ++ drop i r

makeRect :: Int -> Int -> Screen -> Screen
makeRect w h s = fmap (fillLine w) a ++ b
  where (a,b) = splitAt h s

parseOrientation :: Parser Orientation
parseOrientation =
    (string "row" >> return Row) <|> (string "column" >> return Column)

parseInst :: Parser Inst
parseInst = do
  let parseRect = do
        string "rect" >> skipSpace
        Rect <$> decimal <*> (string "x" *> decimal)
      parseRotate = do
        string "rotate" >> skipSpace
        orient <- parseOrientation <* skipSpace
        string "y=" <|> "x="
        Rotate orient <$> decimal <*> (string " by " *> decimal)
  parseRect <|> parseRotate

parseInput :: Text -> [Inst]
parseInput t = rights $ parseOnly parseInst <$> T.lines t

runInst :: Screen -> Inst -> Screen
runInst s (Rect w h) = makeRect w h s
runInst s (Rotate or rc i) = case or of
  Row -> rotateRow rc i s
  Column -> rotateColumn rc i s

go :: Screen -> [Inst] -> Screen
go s is = foldl (\s' i -> runInst s' i) s is

showScreen :: Screen -> String
showScreen s = unlines $ (fmap . fmap) (\c -> if c then '#' else '.') s

main :: IO ()
main = do
  contents <- T.IO.readFile "day8input.txt"
  let insts = parseInput contents
      result = go (blankScreen 6 50) insts
  putStrLn $ showScreen result
  print $ sum $ fmap (length . filter (== True)) result
