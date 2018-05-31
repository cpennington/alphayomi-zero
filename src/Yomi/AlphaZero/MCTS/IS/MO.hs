{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Yomi.AlphaZero.MCTS.IS.MO where

import Yomi.AlphaZero.Types

import Control.Lens (view, over, set, _2, _1)
import Control.Monad (foldM, forM)
import Data.List (maximumBy, (\\), sortBy, find, nub, intercalate, sort)
import Data.Ord (comparing)
import Data.Maybe (catMaybes, fromJust)
import Data.List.NonEmpty (NonEmpty(..), toList)

import Debug.Trace

debug :: (Show p, Show s, Show a) => Node p s a -> String
debug n = concat $
  [ "Node<"
  , show $ view owner n
  , ", "
  , show $ view incoming n
  , "> "
  , show $ view visits n
  , "/"
  , show $ view available n
  , " "
  , show $ view totalR n
  , "["
  , show $ view meanR n
  , "] "
  , show $ view prior n
  , " "
  , show $ ucb1 n
  , "\n"
  ] ++ map (unlines . (map ("  " ++)) . lines . debug) (view children n)

moIsMcts :: (Game g a p, Eq p, Eq a, Ord p, Show p, Show a, Show (PlayerState g), Ord a) => p -> PlayerState g -> Int -> g (Maybe (ActionType a))
moIsMcts currentPlayer playerState iterations = do
    ps <- players
    roots <- forM (Nothing:map Just ps) $ \p -> do
        st <- case p of
            Just p_ -> fmap Just $ stateForPlayer p_
            Nothing -> return Nothing

        return $ unvisitedRoot st p

    let expandOnce rs _ = do
            determine currentPlayer playerState
            expanded <- expandRec rs
            return $ map snd expanded
    roots' <- foldM expandOnce roots (replicate iterations ())

    let opponentRoot = roots' !! 1
        oppChildren = view children opponentRoot

    return $ view incoming $ maximumBy (comparing $ view visits) oppChildren

-- select :: (Game g a p, Eq p, Eq a) => GameForest g a p -> g (GameForest g a p)
-- select game nodes private = do
--     unvisitedActions <- unvisited game nodes private
--     if not $ null unvisitedActions
--         then return (nodes, private)
--         else do
--             st <- currentState game private
--             case st of
--                 DecisionsRequired ((Decision p as) :| _) -> do
--                     let playerNodes = filter ((== p) . (view owner)) nodes
--                     case playerNodes of
--                         [] -> error "No active player node!"
--                         n:_ -> do
--                             let cs = compatibleChildren as n
--                                 best = bestChildUCT cs
--                             private' <- playaame p best
--                             nodes' <- mapM (findOrCreate private') nodes
--                             select game nodes' private'
--                 _ -> return (nodes, private)

ucb1K :: Double
ucb1K = 0.5

ucb1 :: Node p s a -> Double
ucb1 n = mean + ucb1K * sqrt ((log avail) / vis)
    where
        mean = view meanR n
        vis = 1 + (fromIntegral $ view visits n)
        avail = 1 + (fromIntegral $ view available n)

bestChildUCB1 :: (Show p, Show a, Show s, Foldable t) => t (Node p s a) -> ActionType a
bestChildUCB1 cs = case action of
        Nothing -> error "Can't compute UCTB1 for a root node"
        Just a -> a
    where
        action = view incoming $ maximumBy (comparing ucb1) cs

findOrCreate :: (Action a p, Eq a, Show a, Show p, Show s) => Node p s a -> ActionType a -> Node p s a
findOrCreate node incomingAction =
    let p = view owner node
        action' = case incomingAction of
            SingleMove a -> SingleMove $ obscureAction p a
            RevealMoves as -> RevealMoves as
        child = find ((== Just action') . (view incoming)) $ view children node
    in
        case child of
            Just c -> c
            Nothing -> leafNode (view owner node) incomingAction 0

compatibleChildren :: (Show s, Show a, Show p) => Eq a => [ActionType a] -> Node p s a -> [Node p s a]
compatibleChildren as n = filter byIncoming cs
    where
        cs = view children n
        byIncoming c = view incoming c `elem` map Just as

expandWithNetwork :: (Game g a p, Eq p, Eq a) => p -> (PlayerState g) -> g (Reward, [(a, Double)])
expandWithNetwork p _ = do
    gameState <- currentState
    case gameState of
        DecisionsRequired ((Decision _ as) :| _) -> do
            let os = map (obscureAction (Just p)) as
            return (0.5, zip os (repeat (1.0 / (fromIntegral $ length os))))
        TieGame -> return (0, [])
        Victory p' | p == p' -> return (1, [])
        Victory _  | otherwise -> return (-1, [])

expandUnvisited
    :: (Game g a p, Eq a, Eq p, Ord p, Show p, Show a, Show (PlayerState g))
    => GameForest g a p -> g () -> g [(Reward, GameNode g a p)] -> g [(Reward, GameNode g a p)]
expandUnvisited nodes before expandVisited =
    if any (null . view children) nodes
        then do
            before
            -- expand unvisited child nodes via the network
            forM nodes $ \n ->
                if null (view children n)
                    then do
                        let nodeOwner = view owner n
                        case nodeOwner of
                            Nothing -> return (0, visitNode n 0 [])
                            Just o -> do
                                playerState <- stateForPlayer o
                                (v, probabilities) <- expandWithNetwork o playerState
                                let cs' = map (uncurry $ leafNode (Just o)) $ map (over _1 SingleMove) probabilities
                                return $ (v, visitNode n v cs')
                    else
                        return $ (0, visitNode n 0 (view children n))
        else expandVisited

expandSimultaneous
    :: (Game g a p, Eq a, Eq p, Ord p, Ord a, Show p, Show a, Show (PlayerState g))
    => [Decision p a] -> [(Maybe p, ActionType a)] -> GameForest g a p -> g [(Reward, GameNode g a p)]
expandSimultaneous [] pending nodes = do
    let (players', moves) = unzip pending
        actions = map (\(SingleMove a) -> a) moves
    sequence_ $ zipWith playAction players' actions
    let enviroAction = RevealMoves actions
        childNodes = map (flip findOrCreate enviroAction) nodes
    expandedChildren <- expandRec childNodes
    return $ updateNodes nodes [enviroAction] expandedChildren

expandSimultaneous ((Decision p as):ds) pending nodes = do
    let (players', ms) = unzip pending
        actions = map (\(SingleMove a) -> a) ms
    expandUnvisited nodes (sequence_ $ zipWith playAction players' actions) $ do
        let moves = map SingleMove as
            compatible = compatibleChildren moves (fromJust $ nodeForPlayer p nodes)
        if null compatible
            then error $ "No compatible children found: " ++ show (moves, (fromJust $ nodeForPlayer p nodes))
            else do
                -- pick best action
                let best = bestChildUCB1 compatible

                -- find child node for best action for each player
                    childNodes = map (flip findOrCreate best) nodes

                expandedChildren <- expandSimultaneous ds ((p, best):pending) childNodes

                -- update nodes with expanded children
                return $ updateNodes nodes moves expandedChildren

expandRec :: (Game g a p, Eq a, Eq p, Ord p, Show p, Show a, Show (PlayerState g), Ord a) => GameForest g a p -> g [(Reward, GameNode g a p)]
expandRec nodes = expandUnvisited nodes (return ()) $ do
    st <- currentState
    case st of
        DecisionsRequired ((Decision p as) :| []) -> do
            let moves = map SingleMove as
            case compatibleChildren moves (fromJust $ nodeForPlayer p nodes) of
                [] -> error $ "No compatible children found: " ++ show (moves, (fromJust $ nodeForPlayer p nodes))
                cs -> do
                    -- pick best action
                    let best = bestChildUCB1 cs
                        SingleMove bestAction = best
                    -- update the private game state (by playing an action)
                    playAction p bestAction

                    -- find child node for best action for each player
                    let childNodes = map (flip findOrCreate best) nodes

                    expandedChildren <- expandRec childNodes

                    -- update nodes with expanded children
                    return $ updateNodes nodes moves expandedChildren
        DecisionsRequired ds -> expandSimultaneous (sort $ toList ds) [] nodes

        TieGame -> return $ zip (repeat 0.0) nodes
        Victory p' ->
            return $ zip (map (value . (== (Just p')) . view owner) nodes) nodes
                where
                    value True = 1.0
                    value False = 0.0

visitNode :: Node p s a -> Reward -> [Node p s a] -> Node p s a
visitNode parent reward children' =
    set children children' $
    set meanR ((view totalR parent + reward) / (fromIntegral $ view visits parent + 1)) $
    over visits (+1) $
    over totalR (+reward) $
    parent

updateNodes :: forall p s a. (Ord p, Eq a, Action a p, Show a, Show p, Show s) => [Node p s a] -> [ActionType a] -> [(Reward, Node p s a)] -> [(Reward, Node p s a)]
updateNodes parents availableActions expandedChildren = zipWith update sortedParents sortedChildren
    where
        sortedParents = sortBy (comparing (view owner)) parents
        sortedChildren = sortBy (comparing (view (_2 . owner))) expandedChildren
        replaceChild n ns = n : filter ((/= view incoming n) . (view incoming)) ns
        update :: Node p s a -> (Reward, Node p s a) -> (Reward, Node p s a)
        update parent (reward, child) = (reward, parent')
            where
                obscureActions p = flip map availableActions $ \a -> case a of
                    SingleMove m -> SingleMove $ obscureAction p m
                    RevealMoves ms -> RevealMoves ms
                incrementAvailable c = if view incoming c `elem` (map Just $ obscureActions (view owner c))
                    then over available (+1) c
                    else c
                parent' =
                    over children (map incrementAvailable) $
                    visitNode parent reward $ replaceChild child $ view children parent

nodeForPlayer :: (HasOwner n p, Eq p, Show n, Show p) => p -> [n] -> Maybe n
nodeForPlayer p n = traceShow (p, n) $ find ((== p) . view owner) n

unvisited :: (Game g a p, Eq a, Eq p, Ord a, Show p, Show a, Show (PlayerState g)) => GameForest g a p -> g ([ActionType a])
unvisited nodes = do
    st <- currentState
    case st of
        Victory _ -> return []
        TieGame -> return []
        DecisionsRequired ((Decision p as) :| _) -> do
            let playerNode = nodeForPlayer p nodes
            case playerNode of
                Nothing -> error "No active player node!"
                Just n -> do
                    let childActions = catMaybes $ map (view incoming) (view children n)
                    return $ (map SingleMove as) \\ childActions

simulate :: (Game g a p) => g Reward
simulate = error "Implement simulate"

backpropagate :: (Game g a p) => Reward -> GameNode g a p -> g ()
backpropagate = error "Implement backpropagate"
