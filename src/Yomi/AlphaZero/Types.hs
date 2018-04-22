{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}


module Yomi.AlphaZero.Types where

import Control.Lens.TH (makeFields)

type Reward = Double

data Node p s a = Node
    { _nodeState :: s
    , _nodeIncoming :: a
    , _nodeVisits :: Int
    , _nodeAvailable :: Int
    , _nodeReward :: Reward
    , _nodeChildren :: [Node p s a]
    , _nodeOwner :: p
    }
makeFields ''Node

data Tree p s a = Tree
    { _treeVisits :: Int
    , _treeAvailable :: Int
    , _treeReward :: Reward
    , _treeChildren :: [Node p s a]
    , _treeOwner :: p
    }
makeFields ''Tree

type GameNode g = Node (Player g) (PublicState g) (Action g)
type GameTree g = Tree (Player g) (PublicState g) (Action g)

class Game a where
    data PublicState a :: *
    data PrivateState a :: *
    data Action a :: *
    data Player a :: *
    data M a :: * -> *

    players :: a -> (M a) [Player a]
    determine :: a -> PublicState a -> (M a) (PrivateState a)
