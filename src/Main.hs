{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Main where

import Yomi.AlphaZero.Types
import Yomi.AlphaZero.MCTS.IS.MO

import qualified Control.Monad.Trans.State as S
import Data.List.NonEmpty (fromList)
import Control.Lens.TH (makeFields)
import Control.Lens (view, set, over)
import Data.Maybe (isJust)
import Control.Monad.Random (uniform)

data Move = Rock | Paper | Scissors
  deriving (Show, Eq)
data RPS = RPS
  { _rPSP1Move :: (Maybe Move)
  , _rPSP2Move :: (Maybe Move)
  , _rPSP1Score :: Int
  , _rPSP2Score :: Int
  }
  deriving Show

makeFields ''RPS

type RPSGame = S.StateT RPS IO
data RPSAction = RPSAction RPSPlayer (Maybe Move)
  deriving (Show, Eq)
data RPSPlayer = P1 | P2
  deriving (Show, Eq, Ord)

instance Player RPSPlayer

instance Action RPSAction RPSPlayer where
  obscureAction p (RPSAction o _) | p /= o = RPSAction o Nothing
  obscureAction _ a = a

instance Game RPSGame RPSAction RPSPlayer where
  type PlayerState RPSGame = (Maybe Move, Int, Int)

  players = return [P1, P2]
  determine P1 (move, pScore, oScore) = do
    p2Move' <- case move of
      Nothing -> return Nothing
      Just _ -> uniform [Nothing, Just Rock, Just Paper, Just Scissors]
    S.put (RPS move p2Move' pScore oScore)
  determine P2 (move, pScore, oScore) = do
    p1Move' <- case move of
      Nothing -> uniform [Nothing, Just Rock, Just Paper, Just Scissors]
      Just _ -> uniform [Just Rock, Just Paper, Just Scissors]
    S.put (RPS p1Move' move oScore pScore)

  currentState = do
    st <- S.get
    let req f p = if isJust $ view f st
          then []
          else [Decision p $ map (RPSAction p . Just) [Rock, Paper, Scissors]]
    case (view p1Score st, view p2Score st) of
      (p1, p2) | p1 >= 2, p2 < 2 -> return $ Victory P1
      (p1, p2) | p1 < 2, p2 >= 2 -> return $ Victory P2
      (p1, p2) | p1 >= 2, p2 >= 2 -> return TieGame
      _ -> return $ DecisionsRequired $ fromList $ req p1Move P1 ++ req p2Move P2
  playAction p (RPSAction _ (Just a)) = do
    st <- S.get
    let playMove m Nothing = Just m
        playMove _ e = e
        st' = case p of
          P1 -> over p1Move (playMove a) st
          P2 -> over p2Move (playMove a) st
        stReset = set p1Move Nothing $ set p2Move Nothing st'
    case (view p1Move st', view p2Move st') of
      (Just Paper, Just Rock) -> S.put (over p1Score (+1) stReset)
      (Just Rock, Just Scissors) -> S.put (over p1Score (+1) stReset)
      (Just Scissors, Just Paper) -> S.put (over p1Score (+1) stReset)
      (Just Rock, Just Paper) -> S.put (over p2Score (+1) stReset)
      (Just Paper, Just Scissors) -> S.put (over p2Score (+1) stReset)
      (Just Scissors, Just Rock) -> S.put (over p2Score (+1) stReset)
      (Just Rock, Just Rock) -> S.put $ stReset
      (Just Paper, Just Paper) -> S.put $ stReset
      (Just Scissors, Just Scissors) -> S.put $ stReset
      _ -> S.put st'

  playAction _ (RPSAction _ Nothing) = return ()
  stateForPlayer p = do
    st <- S.get
    case p of
      P1 -> return (view p1Move st, view p1Score st, view p2Score st)
      P2 -> return (view p2Move st, view p2Score st, view p1Score st)

main :: IO ()
main = do
  result <- S.evalStateT (moIsMcts P1 (Nothing, 0, 0) 30) (RPS Nothing Nothing 0 0)
  print result
