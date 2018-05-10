{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Yomi.AlphaZero.MCTS.IS.MO where

import Yomi.AlphaZero.Types

import Control.Lens (view, over, set, _2, _1)
import Control.Monad (foldM, forM)
import Data.List (maximumBy, (\\), sortBy, find, nub, intercalate)
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

moIsMcts :: (Game g a p, Eq p, Eq a, Ord p, Show p, Show a, Show (PlayerState g)) => p -> PlayerState g -> Int -> g (Maybe (ActionType a))
moIsMcts currentPlayer playerState iterations = do
    ps <- players
    roots <- forM ps $ \p -> do
        st <- stateForPlayer p
        return $ unvisitedRoot st p

    let expandOnce rs _ = do
            determine currentPlayer playerState
            expanded <- expandRec $ trace (intercalate "\n" $ map debug rs) rs
            return $ map snd expanded
    roots' <- foldM expandOnce roots (replicate iterations ())

    let opponentRoot = (trace (concatMap debug roots') roots') !! 1
        oppChildren = view children opponentRoot

    return $ view incoming $ maximumBy (comparing $ view visits) oppChildren

-- select :: (Game g a p, Eq p, Eq a) => [GameNode g a p] -> g ([GameNode g a p])
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

bestChildUCB1 :: (Show p, Show a, Show s) => [Node p s a] -> ActionType a
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
            let os = nub $ map (obscureAction p) as
            return (0.5, zip os (repeat (1.0 / (fromIntegral $ length os))))
        TieGame -> return (0, [])
        Victory p' | p == p' -> return (1, [])
        Victory _  | otherwise -> return (-1, [])

expandUnvisited
    :: (Game g a p, Eq a, Eq p, Ord p, Show p, Show a, Show (PlayerState g))
    => [GameNode g a p] -> g () -> g [(Reward, GameNode g a p)] -> g [(Reward, GameNode g a p)]
expandUnvisited nodes before expandVisited =
    if any (null . view children) nodes
        then do
            before
            -- expand unvisited child nodes via the network
            forM nodes $ \n ->
                if null (view children n)
                    then do
                        let nodeOwner = view owner n
                        playerState <- stateForPlayer nodeOwner
                        (v, probabilities) <- expandWithNetwork nodeOwner playerState
                        let cs' = map (uncurry $ leafNode nodeOwner) $ map (over _1 SingleMove) probabilities
                        return $ (v, visitNode n v cs')
                    else
                        return $ (0, visitNode n 0 (view children n))
        else expandVisited

expandSimultaneous
    :: (Game g a p, Eq a, Eq p, Ord p, Show p, Show a, Show (PlayerState g))
    => [Decision p a] -> [(p, ActionType a)] -> [GameNode g a p] -> g [(Reward, GameNode g a p)]
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
        case compatibleChildren moves (fromJust $ nodeForPlayer p nodes) of
            [] -> error $ "No compatible children found: " ++ show (moves, (fromJust $ nodeForPlayer p nodes))
            cs -> do
                -- pick best action
                let best = bestChildUCB1 cs

                -- find child node for best action for each player
                    childNodes = map (flip findOrCreate best) nodes

                expandedChildren <- expandSimultaneous ds ((p, best):pending) childNodes

                -- update nodes with expanded children
                return $ updateNodes nodes moves expandedChildren

expandRec :: (Game g a p, Eq a, Eq p, Ord p, Show p, Show a, Show (PlayerState g)) => [GameNode g a p] -> g [(Reward, GameNode g a p)]
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
        DecisionsRequired ds -> expandSimultaneous (toList ds) [] nodes

        TieGame -> return $ zip (repeat 0.0) nodes
        Victory p' ->
            return $ zip (map (value . (== p') . view owner) nodes) nodes
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

nodeForPlayer :: (HasOwner n p, Eq p) => p -> [n] -> Maybe n
nodeForPlayer p = find ((== p) . view owner)

unvisited :: (Game g a p, Eq a, Eq p) => [GameNode g a p] -> g [ActionType a]
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
