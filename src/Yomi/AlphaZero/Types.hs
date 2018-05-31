{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}


module Yomi.AlphaZero.Types where

import Control.Lens.TH (makeFields)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Random.Source (MonadRandom)

type Reward = Double
data ActionType a = SingleMove a | RevealMoves [a]
    deriving (Eq, Ord, Show)

data Node p s a = Node
    { _nodeState :: Maybe s
    , _nodeIncoming :: Maybe (ActionType a)
    , _nodeVisits :: Int
    , _nodeAvailable :: Int
    , _nodeTotalR :: Reward
    , _nodeMeanR :: Reward
    , _nodePrior :: Double
    , _nodeChildren :: [Node p s a]
    , _nodeOwner :: Maybe p
    }
    deriving (Eq, Ord, Show)

makeFields ''Node

unvisitedRoot :: Maybe s -> Maybe p -> Node p s a
unvisitedRoot s p = Node s Nothing 0 0 0 0 0 [] p

leafNode :: Maybe p -> ActionType a -> Double -> Node p s a
leafNode p a pr = Node Nothing (Just a) 0 0 0 0 pr [] p

data State p a
    = Victory p
    | TieGame
    | DecisionsRequired (NonEmpty (Decision p a))
    deriving (Show, Eq)

data Decision p a = Decision (Maybe p) [a]
    deriving (Show, Ord, Eq)

type GameNode g a p = Node p (PlayerState g) a
type GameForest g a p = [GameNode g a p]

class Player p

class Player p => Action a p where
    obscureAction :: (Maybe p) -> a -> a

class (Monad g, Action a p, Player p, MonadRandom g) => Game g a p | g -> a p where
    type PlayerState g :: *

    players :: g [p]
    determine :: p -> PlayerState g -> g ()
    currentState :: g (State p a)
    playAction :: (Maybe p) -> a -> g ()
    stateForPlayer :: p -> g (PlayerState g)
