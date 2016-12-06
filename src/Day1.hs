{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad

import Data.Maybe (mapMaybe)
import Data.List

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Attoparsec.Text

data Dir = N | E | S | W deriving (Eq, Ord, Show)

left :: Dir -> Dir
left N = W
left E = N
left S = E
left W = S

right :: Dir -> Dir
right N = E
right E = S
right S = W
right W = N

data Inst = L Int | R Int deriving (Eq, Ord, Show)

instLen :: Inst -> Int
instLen (L i) = i
instLen (R i) = i

turn :: Dir -> Inst -> Dir
turn d (L _) = left d
turn d (R _) = right d

walk :: (Int, Int) -> Dir -> Int -> [(Int, Int)]
walk (x,y) N i = reverse $ [(x,y') | y' <- [y+1..y+i]]
walk (x,y) E i = reverse $ [(x',y) | x' <- [x+1..x+i]]
walk (x,y) S i = [(x,y') | y' <- [y-i..y-1]]
walk (x,y) W i = [(x',y) | x' <- [x-i..x-1]]


go :: [(Int, Int)] -> Dir -> Int -> [(Int, Int)]
go [] _ _ = []
go steps d i = walk (head steps) d i ++ steps


interpretInsts :: [Inst] -> [(Int, Int)]
interpretInsts is = reverse $ fst $
  foldl (\(steps, dir) inst ->
           let dir' = turn dir inst in
               (go steps dir' (instLen inst), dir')
        ) ([(0,0)], N) is


instParser :: Parser Inst
instParser = do
  t <- (char 'L' >> return L) <|> (char 'R' >> return R)
  skipSpace
  i <- decimal
  return $ t i

-- instsParser :: Parser [Inst]
-- instsParser = many' $ instParser <* char ',' <* skipSpace
-- instsParser = many' $ instParser

parseInst :: Text -> Maybe Inst
parseInst t = do
  guard $ T.length t > 1
  let n = T.tail t
  case T.head t of
    'L' -> Just $ L $ read $ T.unpack n
    'R' -> Just $ R $ read $ T.unpack n
    _ -> Nothing

parseInsts :: Text -> [Inst]
parseInsts t = mapMaybe parseInst $ T.splitOn ", " t

run :: Set (Int, Int) -> [(Int, Int)] -> Maybe (Int, Int)
run _ [] = Nothing
run vs (c:cs) = case Set.member c vs of
  True -> Just c
  False -> run (Set.insert c vs) cs

dist :: (Int, Int) -> (Int, Int) -> Int
dist (x1,y1) (x2,y2) = abs (x2 - x1) + abs (y2 - y1)

main :: IO ()
main = do
  contents <- TIO.readFile "day1input.txt"
  print $ length $ T.splitOn ", " contents
  print $ length $ parseInsts contents
  let insts = parseInsts contents
      coords = interpretInsts insts
      result = run Set.empty coords
  print coords
  putStrLn "------"
  putStrLn "RESULT"
  putStrLn "------"
  print result
  case result of
    Just c -> print $ dist c (0,0)
    Nothing -> print "Nothing"
