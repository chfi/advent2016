{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Monad

import           Data.Char                  (digitToInt)
import           Data.Either                (rights)
import           Data.Maybe                 (catMaybes, isJust, isNothing)

import           Data.List                  (isPrefixOf)
import           Data.Monoid                ((<>))

import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8 as C

import           Data.Digest.Pure.MD5

import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Data.Attoparsec.Text       hiding (take)

import           System.Console.ANSI
import           System.IO
import           System.Random

type Password = [Maybe Char]
newtype PWState = PWState (MVar Password)

doorID :: ByteString
doorID = "ffykfhsq"

testID :: ByteString
testID = "abc"

hashParserA :: Parser Char
hashParserA = do
  string "00000"
  anyChar

hashParserB :: Parser (Int, Char)
hashParserB = do
  string "00000"
  pos <- digitToInt <$> digit
  guard $ pos < 8
  c <- anyChar
  return $ (pos, c)

updatePW :: Password -> Char -> Int -> Password
updatePW pw c i = if isJust (pw !! i) then pw else p ++ [(Just c)] ++ s
  where (p, (_:s)) = splitAt i pw

displayPassword :: Int -> Int -> Password -> IO ()
displayPassword r c pw = do
  let rc = (!!) (['0'..'9'] ++ ['a'..'f']) <$> (randomRIO (0,15))
  str <- mapM (\c -> case c of
                     Nothing -> rc
                     Just c -> return c) pw
  setCursorPosition r c
  putStrLn str

getHashes :: Parser a -> ByteString -> [a]
getHashes p bs = rights $ fmap (\i -> parseOnly p (go i)) [1..]
  where go i = T.pack $ show $ md5 $ bs <> (C.pack . show) i

getHashesA :: ByteString -> [Char]
getHashesA = getHashes hashParserA

getHashesB :: ByteString -> [(Int, Char)]
getHashesB = getHashes hashParserB

updatePWState :: PWState -> Char -> Int -> IO ()
updatePWState (PWState m) c i = do
  pw <- takeMVar m
  let pw' = updatePW pw c i
  putMVar m pw'
  setCursorPosition 8 4

mainLoop :: PWState -> IO ()
mainLoop pws@(PWState m) = do
  threadDelay $ 30000
  pw <- readMVar m
  displayPassword 5 10 pw
  if null $ filter isNothing pw
    then return ()
    else mainLoop pws

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  pwvar <- newMVar $ take 8 $ repeat $ Nothing
  let pwstate = PWState pwvar

  clearScreen

  setCursorPosition 1 1
  putStr "STARTING THREADS"

  -- PART A
  _ <- forkIO $ mapM_ (\(p,c) -> forkIO $ updatePWState pwstate c p) $ zip [0..]$ getHashesA doorID

  -- PART B
  _ <- forkIO $ mapM_ (\(p,c) -> forkIO $ updatePWState pwstate c p) $ getHashesB doorID

  setCursorPosition 3 8
  putStr "DECRYPTING PASSWORD"
  mainLoop pwstate
