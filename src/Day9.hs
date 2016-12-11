{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (take)
import Data.Monoid ((<>))

import Control.Applicative

import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T.IO

import           Data.Attoparsec.Text

import System.Environment (getArgs)

data Comp
    = MComp Int
            Comp
    | TComp Text
    | CComp Comp
            Comp
    | NComp
    deriving (Eq,Show)


compP :: Parser Comp
compP = do
  let mcompP = do
        x <- "(" *> decimal
        y <- "x" *> decimal <* ")"
        t <- take x
        pure $ MComp y (TComp t)
      tcompP = do
        t <- takeWhile1 (/= '(')
        pure $ TComp t
  mcompP <|> tcompP

expand :: Comp -> Comp
expand (TComp t) = case parseOnly (many' compP) $ t of
  Left _ -> TComp t
  Right t' -> foldr (\comp acc -> case comp of
                        tc@(TComp _) -> CComp acc tc
                        (MComp i c) -> CComp acc (MComp i (expand c))
                        _ -> acc) (CComp NComp NComp) t'
expand (MComp i c) = (MComp i (expand c))
expand (CComp a b) = CComp (expand a) (expand b)


compLen :: Comp -> Int
compLen NComp = 0
compLen (TComp t) = T.length t
compLen (CComp a b) = compLen a + compLen b
compLen (MComp i c) = i * compLen c


main :: IO ()
main = do
  contents <- T.IO.readFile =<< head <$> getArgs
  case parseOnly (many' compP) (T.strip contents) of
    Left e -> print $ "parse failed: " <> e
    Right c -> print $ sum $ fmap (compLen . expand) c
