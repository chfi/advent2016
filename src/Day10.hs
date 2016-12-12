{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.IntMap.Strict   (IntMap, (!))
import qualified Data.IntMap.Strict   as IntMap
import           Data.List            (intersect, partition, sort)
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T.IO
import           System.Environment   (getArgs)
import           Prelude              hiding (take)

type Chip = Int
data Bot = Bot (Maybe Chip) (Maybe Chip) Inst [Chip] deriving (Eq)

instance Show Bot where
  show (Bot Nothing Nothing i mem) = "(Bot _ _ " <> show mem <> ")"
  show (Bot (Just l) Nothing i mem) = "(Bot " <> show l <> " _ " <> show mem <> ")"
  show (Bot Nothing (Just h) i mem) = "(Bot _ " <> show h <> " " <> show mem <> ")"
  show (Bot (Just l) (Just h) i mem) = "(Bot " <> show l <> " " <> show h <> " " <> show mem <> ")"

type BotNum = Int
type Output = Maybe Chip
data Inst = Init BotNum Chip | Give BotNum Target Target deriving (Eq, Show)

data Target = ToBot Int | ToOutput Int deriving (Eq, Show)

type State = (IntMap Bot, IntMap Chip)

instP :: Parser Inst
instP = do
  let initP = do
        "value "
        val <- decimal
        " goes to bot "
        bot <- decimal
        pure $ Init bot val
      giveP = do
        "bot "
        bot <- decimal
        " gives low to "
        low <- ("bot " >> pure ToBot) <|> ("output " >> pure ToOutput)
        loOut <- low <$> decimal
        " and high to "
        high <- ("bot " >> pure ToBot) <|> ("output " >> pure ToOutput)
        hiOut <- high <$> decimal
        pure $ Give bot loOut hiOut
  initP <|> giveP


stepBot :: Bot -> State -> State
stepBot b@(Bot Nothing _ _ _) st = st
stepBot b@(Bot _ Nothing _ _) st = st
stepBot b@(Bot (Just l) (Just h) i mem) st =
  case i of
    Give n lt ht -> (bs'', os')
      where (bs', os') = giveChip h ht (giveChip l lt st)
            bs'' = IntMap.insert n (Bot Nothing Nothing i mem) bs'
    Init _ _ -> st

giveToBot :: Chip -> Bot -> Bot
giveToBot c (Bot Nothing Nothing i mem) =
  Bot (Just c) Nothing i (c:mem)
giveToBot c (Bot (Just a) Nothing i mem) =
  if a > c then Bot (Just c) (Just a) i (c:mem)
           else Bot (Just a) (Just c) i (c:mem)
giveToBot c (Bot Nothing (Just a) i mem) =
  giveToBot c (Bot (Just a) Nothing i mem)

giveToOutput :: Chip -> Output -> Output
giveToOutput c _ = Just c


giveChip :: Chip -> Target -> State -> State
giveChip c (ToBot i) (bots, outputs) = (bots' , outputs)
  where bots' = IntMap.adjust (giveToBot c) i bots
giveChip c (ToOutput i) (bots, outputs) = (bots, outputs')
  where outputs' = IntMap.insert i c outputs

initState :: [Inst] -> State
initState is = initVals
  where (val, inst) = partition (\i -> case i of
                             Init _ _ -> True
                             _ -> False
                         ) is
        initBots = foldr (\i bs -> case i of
                             Give n lt ht ->
                               IntMap.insert n (Bot Nothing Nothing i []) bs) IntMap.empty inst
        initVals = foldr (\i s -> case i of
                             Init bn c -> giveChip c (ToBot bn) s) (initBots, IntMap.empty) val


botCanMove :: Bot -> Bool
botCanMove (Bot Nothing _ _ _) = False
botCanMove (Bot _ Nothing _ _) = False
botCanMove _ = True

findMoveableBotId :: State -> Maybe Int
findMoveableBotId (bs,os) =
    if null filtered
        then Nothing
        else Just $ fst $ IntMap.findMin $ filtered
  where
    filtered =
        IntMap.mapMaybeWithKey
            (\bnum bot ->
                  if botCanMove bot
                      then Just bnum
                      else Nothing)
            bs

runProblem :: State -> State
runProblem s@(bs, os) = case findMoveableBotId s of
  Nothing   -> s
  Just bnum -> runProblem $ stepBot bot s
    where bot = bs ! bnum

findRespBot :: IntMap Bot -> Chip -> [(Int, Bot)]
findRespBot bs c = IntMap.toAscList $ IntMap.filter (\(Bot _ _ _ mem) -> c `elem` mem) bs

main :: IO ()
main = do
  contents <- T.IO.readFile =<< head <$> getArgs
  case parseOnly (many' (instP <* endOfLine)) contents of
    Left e -> print e
    Right is -> do
      let state = initState is
      -- print is
      print state
      print $ findMoveableBotId state
      print $ fmap botCanMove $ fst state
      let s'@(bs, os) = runProblem state
      print $ s'
      print "--------------"
      print $ findRespBot bs 61 `intersect` findRespBot bs 17
      print $ do
        a <- IntMap.lookup 0 os
        b <- IntMap.lookup 1 os
        c <- IntMap.lookup 2 os
        pure $ a * b * c
      pure ()
