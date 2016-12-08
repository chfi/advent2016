{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.List            (sort, transpose)
import           Data.Attoparsec.Text hiding (take)
import           Data.Maybe           (mapMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T.IO
import           System.Environment   (getArgs)

newtype Tri = Tri (Int, Int, Int) deriving (Eq, Show)

tri :: (Int, Int, Int) -> Tri
tri (a,b,c) = Tri (s,m,l)
  where [s,m,l] = sort [a,b,c]

tryTri :: [Int] -> Maybe Tri
tryTri e@[_,_,_] = Just $ Tri (s,m,l)
  where [s,m,l] = sort e
tryTri _ = Nothing

isLegal :: Tri -> Bool
isLegal (Tri (s,m,l)) = (s + m) > l

parseInput :: Parser [Tri]
parseInput = many' (parseTri <* endOfLine) <* endOfInput

parseTri :: Parser Tri
parseTri = do
  let piece = skipSpace >> decimal
      line = (,,) <$> piece <*> piece <*> piece <* endOfLine
  tri <$> line

parseTris :: Parser [Tri]
parseTris = do
  let piece = skipSpace >> decimal
      line = count 3 piece <* endOfLine
  mapMaybe tryTri <$> transpose <$> count 3 line

main :: IO ()
main = do
  contents <- T.IO.readFile =<< head <$> getArgs

  case parseOnly (many' parseTri) contents of
    Left e -> print e
    Right ts -> print $ length $ filter isLegal ts

  case parseOnly (many' parseTris) contents of
    Left e -> print e
    Right ts -> print $ length $ filter isLegal $ concat ts
