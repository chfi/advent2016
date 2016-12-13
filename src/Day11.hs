{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.List            (intersect, partition, sort)
import           Data.Monoid          ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T.IO
import           System.Environment   (getArgs)
import           Prelude              hiding (take)


data Mineral = Strontium
             | Plutonium
             | Thulium
             | Ruthenium
             | Curium
             deriving (Eq, Show, Ord, Enum, Bounded)

-- data Device = Generator Mineral | Microchip Mineral deriving (Eq, Show)
newtype Microchip = Microchip Mineral deriving (Eq, Show, Ord, Enum, Bounded)
newtype Generator = Generator Mineral deriving (Eq, Show, Ord, Enum, Bounded)


data FloorNum = F1 | F2 | F3 | F4 deriving (Eq, Ord, Enum, Bounded, Show)
type ElevatorFloor = FloorNum

  -- should a floor have a number in it? or should it be referred to using its number
  -- the latter feels more appropriate
data Floor = Floor [Microchip] [Generator] deriving (Eq, Show)
newtype Area = Area (Floor, Floor, Floor, Floor)

type State = (Area, FloorNum)

  -- The first floor contains a strontium generator, a strontium-compatible microchip,
  -- a plutonium generator, and a plutonium-compatible microchip.
initF1 :: Floor
initF1 = Floor [Microchip Strontium, Microchip Plutonium]
               [Generator Strontium, Generator Plutonium]


 -- The second floor contains a thulium generator, a ruthenium generator, a
 -- ruthenium-compatible microchip, a curium generator, and a curium-compatible microchip.
initF2 :: Floor
initF2 = Floor [Microchip Ruthenium, Microchip Curium]
               [Generator Ruthenium, Generator Curium, Generator Thulium]

 -- The third floor contains a thulium-compatible microchip.
initF3 :: Floor
initF3 = Floor [Microchip Thulium] []

 -- The fourth floor contains nothing relevant.
initF4 :: Floor
initF4 = Floor [] []

initState :: State
initState = (Area (initF1, initF2, initF3, initF4), F1)


goalF4mcs = (sort [Microchip minBound .. Microchip maxBound])

stateIsFinished :: State -> Bool
stateIsFinished (Area (_, _, _, Floor mcs gens), F4) = (sort mcs) == goalF4mcs

chipIsSafe :: Microchip -> [Generator] -> Bool
chipIsSafe (Microchip m) s =
  (Generator m) `elem` s || null s


-- a move is basically moving the elevator up or down and moving one or two parts.

type Move = State -> (Either Microchip Generator, Either Microchip Generator) -> State

-- legalMoves :: State -> [State]
-- legalMoves

-- the goal: get all chips and generators to the 4th floor
-- rules: chips are fried if adjacent to any other generator than the correct one;
--        however, if connected to own generator, they're safe.
-- can only move two things in the elevator, and there needs to be one thing in the elevator
-- at all times
-- can't move without the elevator

-- from a given state --- i.e. position of elevator and four floors --- find a way
-- to list all possible moves. then just search it

-- the elevator can only move to adjacent floors...





main :: IO ()
main = print "day 11"
