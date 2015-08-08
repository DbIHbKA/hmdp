-- | Dynamic Programming module.
module DP where

import Data.Map (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Numeric.LinearAlgebra.Data
       (Matrix, Vector, fromList, (!), toRows, (¿))
import Numeric.LinearAlgebra.HMatrix as H

import MDP
import Planning


data MDPM s a = MDPM
    { statesm :: Map s Int
    , actionsm :: Map a Int
    , transitionm :: Map a (Matrix Double)
    , rewardm :: Matrix Double
    , gammam :: Double             -- discount
    , sizeStates :: Int           -- S
    , sizeActions :: Int          -- A
    }


prepareMDP :: (Ord s, Ord a) => MDP s a -> MDPM s a
prepareMDP mdp =
    MDPM
    { statesm = statesM
    , actionsm = actionsM
    , transitionm = transitionM
    , rewardm = rewardM
    , gammam = gamma mdp
    , sizeStates = S.size (states mdp)
    , sizeActions = S.size (actions mdp)
    }
  where
    statesM = M.fromList (zip (S.toList (states mdp)) [0 ..])
    actionsM = M.fromList (zip (S.toList (actions mdp)) [0 ..])
    transitionM = transitionMap
    transitionMap =
        M.fromList
            (map
                 (\a ->
                       (a, createMatrix a))
                 (S.toList (actions mdp)))
    createMatrix a =
        H.fromLists
            (map
                 (\s ->
                       map (transition mdp a s) (S.toList (states mdp)))
                 (S.toList (states mdp)))
    rewardM = createRewardMatrix
    createRewardMatrix =
        H.fromLists
            (map
                 (\s ->
                       map
                           (\a ->
                                 reward mdp a s)
                           (S.toList (actions mdp)))
                 (S.toList (states mdp)))


valueIterationM :: (Ord a) => MDPM s a -> Int -> Matrix Double -> Matrix Double
valueIterationM mdpm k v
  | k == 0 = v
  | otherwise = valueIterationM mdpm (k - 1) (calc v)
  where
    calc v' =
        rewardm mdpm +
        gammam mdpm `H.scale`
        fromColumns
            (map
                 (\a ->
                       flatten
                           (transitionm mdpm M.! a H.<>
                            (v' ¿ [actionsm mdpm M.! a])))
                 (M.keys (actionsm mdpm)))


valueIteration :: (Ord a) => MDP s a -> Int -> s -> a
valueIteration mdp k s = undefined
