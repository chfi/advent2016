{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Applicative

import Data.Maybe (mapMaybe, catMaybes)
import Data.Either (lefts, rights)

import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T.IO

import           Data.Attoparsec.Text hiding (take)

data IPv7 = IPv7 [Text] [Text] deriving (Eq, Show)

ipv7Parser :: Parser IPv7
ipv7Parser = do
  ts <- takeWhile1 (\c -> c /= '[' && c /= ']')
          `sepBy` (string "[" <|> string "]")
  let (a, b) = alternate ts
  return $ IPv7 a b

alternate :: [a] -> ([a], [a])
alternate as = (ls as', rs as')
  where ls x = fmap fst $ filter (odd  . snd) x
        rs x = fmap fst $ filter (even . snd) x
        as' = zip as [1..]

parseInput :: Text -> [IPv7]
parseInput t = rights $ parseOnly ipv7Parser <$> T.lines t

isABBA :: Text -> Bool
isABBA t
  | T.length t /= 4 = False
  | otherwise = t == T.reverse t && T.head t /= T.head (T.tail t)

hasABBA :: Text -> Bool
hasABBA = or . (concatMap . fmap) isABBA . fmap (T.chunksOf 4) . take 4 . T.tails

getABA :: Text -> Maybe Text
getABA t
  | T.length t /= 3 = Nothing
  | otherwise = if t == T.reverse t && T.head t /= T.head (T.tail t)
                then Just t
                else Nothing

findABA :: Text -> [Text]
findABA = catMaybes . (concatMap . fmap) getABA . fmap (T.chunksOf 3) . take 4 . T.tails

hasBABs :: Text -> [Text] -> Bool
hasBABs t babs = any (\b -> b `T.isInfixOf` t) babs

abaToBAB :: Text -> Text
abaToBAB aba = T.snoc t h
  where t = T.tail aba
        h = T.head t

tlsEnabled :: IPv7 -> Bool
tlsEnabled (IPv7 a b) = (any hasABBA a) && not (any hasABBA b)

sslEnabled :: IPv7 -> Bool
sslEnabled (IPv7 s h) = any (\t -> hasBABs t babs) h
  where abas = concatMap findABA s
        babs = fmap abaToBAB abas

main :: IO ()
main = do
  contents <- T.IO.readFile "day7input.txt"
  let ips = parseInput contents
      tls = filter (== True) $ fmap tlsEnabled ips
      ssl = filter (== True) $ fmap sslEnabled ips
  print $ length tls
  print $ length ssl
