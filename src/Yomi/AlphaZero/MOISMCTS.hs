{-# LANGUAGE FlexibleContexts #-}

module Yomi.AlphaZero.MOISMCTS where

import Yomi.AlphaZero.Types

import Control.Lens (view)
import Control.Monad (replicateM_, mapM)
import Data.List (maximumBy)
import Data.Ord (comparing)

mo_is_mcts :: (Game g, Monad (M g)) => g -> PublicState g -> Int -> (M g) (Action g)
mo_is_mcts game public iterations = do
    ps <- players game
    let roots = map (Tree 0 0 0 []) ps

    replicateM_ iterations $ do
        private0 <- determine game public
        (endNodes, private) <- select roots private0
        unvisitedActions <- unvisited game endNodes private
        (endNodes', private') <- if not $ null unvisitedActions
            then expand endNodes private
            else return (endNodes, private)
        r <- simulate game private'
        mapM (backpropagate r) endNodes'

    return $ view incoming $ maximumBy (comparing $ view visits) (view children $ roots !! 1)

select ::(Game g, Monad (M g)) => [GameTree g] -> PrivateState g -> (M g) ([GameNode g], PrivateState g)
select = error "Implement select"

expand ::(Game g, Monad (M g)) => [GameNode g] -> PrivateState g -> (M g) ([GameNode g], PrivateState g)
expand = error "Implement expand"

unvisited ::(Game g, Monad (M g)) => g -> [GameNode g] -> PrivateState g -> (M g) [Action g]
unvisited = error "Implement unvisited"

simulate ::(Game g, Monad (M g)) => g -> PrivateState g -> (M g) Reward
simulate = error "Implement simulate"

backpropagate ::(Game g, Monad (M g)) => Reward -> GameNode g -> (M g) ()
backpropagate = error "Implement backpropagate"
