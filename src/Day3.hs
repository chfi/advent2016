{-# LANGUAGE OverloadedStrings #-}

module Main where


import Control.Monad


import           Data.Attoparsec.Text

import Data.List (sort, transpose)

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import System.Environment (getArgs)

newtype Tri = Tri (Int, Int, Int) deriving (Eq, Show)

newTri :: [Int] -> Maybe Tri
newTri e@[_,_,_] = Just $ Tri (s,m,l)
  where [s,m,l] = sort e
newTri _ = Nothing

isLegal :: Tri -> Bool
isLegal (Tri (s,m,l)) = (s + m) > l

parseTri :: Text -> Maybe Tri
parseTri t = newTri $ fmap (read . T.unpack) $ T.words t

limList :: [a] -> [[a]]
limList [] = []
limList ts = h : limList t
  where (h, t) = splitAt 3 ts

parseWords :: Text -> [[Text]]
parseWords t = fmap T.words $ T.lines t

transposeInput :: Text -> Text
transposeInput t = T.unlines $ fmap T.unwords $ transpose $ fmap T.words $ T.lines t

parseInput :: Text -> [Tri]
parseInput t = mapMaybe parseTri $ T.lines t

parseInput' :: Parser [Tri]
parseInput' = many' (parseTri' <* endOfLine) <* endOfInput


parseTri' :: Parser Tri
parseTri' = do
  skipSpace
  a <- decimal
  skipSpace
  b <- decimal
  skipSpace
  c <- decimal
  let [s,m,l] = sort [a,b,c]
  return $ Tri (s,m,l)



main :: IO ()
main = do
  contents <- T.IO.readFile =<< head <$> getArgs

  case parseOnly parseInput' contents of
    Left e -> print e
    Right ts -> print ts

  -- let tris' = case parseInput' contents
  --       Done _ ts -> Just ts
  --       _ -> Nothing

  -- let t'd = fmap T.words $ T.lines $ transposeInput $ contents
  --     t'd2 = fmap limList t'd
  --     tris = (concatMap . mapMaybe) (parseTri . T.unwords) t'd2

  -- print t'd
  -- print t'd2
  -- print tris
  -- print $ length $ filter isLegal tris
