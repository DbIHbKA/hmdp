-- | Planning by Dynamic Programming (Matrix Version)

module PlanningM where

import           Data.Map                      (Map)
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Numeric.LinearAlgebra.Data    (Matrix, Vector, (!))
import           Numeric.LinearAlgebra.HMatrix as H

import           MDP
import           Planning

data MDPM s a = MDPM
    { statesm     :: Map s Int
    , actionsm    :: Map a Int
    , transitionm :: Matrix Double -- matrix S x S
    , rewardm     :: Vector Double     -- vector 1 x S
    , gammam      :: Double             -- discount
    , sizeStates  :: Int           -- S
    , sizeActions :: Int          -- A
    }

type PolicyM = Matrix Double


policyEvaluationM :: (Ord s, Ord a) => MDP s a -> ValueFunction s a
policyEvaluationM mdp policy k s =
    matrixPolicyEvaluation mdpm k ! (M.!) (statesm mdpm) s
  where
    mdpm = prepareMDP mdp (preparePolicy mdp policy)


preparePolicy :: MDP s a -> Policy s a -> PolicyM
preparePolicy mdp policy = H.fromLists (
  map (
    \s -> map (policy s) (S.toList (actions mdp)))
  (S.toList (states mdp)))


prepareMDP :: (Ord s, Ord a) => MDP s a -> PolicyM -> MDPM s a
prepareMDP mdp policy =
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
    transitionM =
        H.fromRows
            (map
                 (\s ->
                       takeDiag (policy H.<> (transitionMap M.! s)))
                 (S.toList (states mdp)))
    transitionMap =
        M.fromList
            (map
                 (\s ->
                       (s, createMatrix s))
                 (S.toList (states mdp)))
    createMatrix s' =
        H.fromLists
            (map
                 (\a ->
                       map
                           (\s ->
                                 transition mdp a s s')
                           (S.toList (states mdp)))
                 (S.toList (actions mdp)))
    rewardM = takeDiag (policy H.<> createRewardMatrix)
    createRewardMatrix =
        H.fromLists
            (map
                 (\a ->
                       map
                           (
                                 reward mdp a)
                           (S.toList (states mdp)))
                 (S.toList (actions mdp)))


matrixPolicyEvaluation :: MDPM s a -> Int -> Vector Double
matrixPolicyEvaluation mdpm = evalVF (konst 0.0 (sizeStates mdpm))
  where
    evalVF v 0 = v
    evalVF v k =
        evalVF
            (rewardm mdpm + gammam mdpm `scale` (transitionm mdpm H.#> v))
            (k - 1)


matrixPolicyImprove :: MDPM s a -> Vector Double -> Matrix Double
matrixPolicyImprove mdpm vf = undefined
