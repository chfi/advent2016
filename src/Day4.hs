{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Attoparsec.Text hiding (take)
import           Data.Char            (GeneralCategory (..), chr,
                                       generalCategory, isAlpha, isUpper, ord)
import           Data.List            (sort, sortBy, transpose)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (mapMaybe)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T.IO

import System.Environment (getArgs)

data Room = Room Text Int Text deriving (Eq, Show)

roomParser :: Parser Room
roomParser = do
  n <- sepBy (takeWhile1 isAlpha) (char '-')
  char '-'
  id <- decimal
  chs <- char '[' *> takeWhile1 isAlpha <* char ']'
  return $ Room (T.concat n) id chs

parseRoom :: Text -> Maybe Room
parseRoom t = case parse roomParser t of
  Done _ r -> Just r
  _        -> Nothing

parseInput :: Text -> [Room]
parseInput t = mapMaybe parseRoom $ T.lines t

occurrences :: (Ord a) => [a] -> Map a Int
occurrences = foldr (\a m -> Map.insertWith (\_ o -> (o + 1)) a 1 m) Map.empty

calcChecksum :: Text -> Text
calcChecksum t = T.pack t'
  where occ = occurrences $ T.unpack t
        t' = take 5 $ fmap fst $
             sortBy (\(a,i) (b,j) -> j `compare` i <> a `compare` b) $ Map.toList occ

isRealRoom :: Room -> Bool
isRealRoom (Room name _ chk) = calcChecksum name == chk

decrypt :: Int -> Text -> Text
decrypt i = T.map (\c -> ['a'..'z'] !! ((ord c - ord 'a' + i) `mod` 26)) . T.toLower

decryptRoom :: Room -> (Text, Int)
decryptRoom (Room n s _) = (decrypt s n, s)

main :: IO ()
main = do
  contents <- T.IO.readFile =<< head <$> getArgs

  let rooms = parseInput contents
      real = filter isRealRoom rooms
      idSum = foldr (\(Room _ sid _) s -> sid + s) 0 real
      dec = fmap decryptRoom real
      final = filter (\(n,_) -> T.isInfixOf "north" n) dec

  print dec
  print final
