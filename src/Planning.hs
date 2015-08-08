-- | Planning by Dynamic Programming

module Planning where

import qualified Data.Set as S
import MDP


type Policy s a = s -> a -> Double  -- ^ distribution over actions given states

type ValueFunction s a = Policy s a -> Int -> s -> Double -- ^ Expected return



policyEvaluation :: MDP s a -> ValueFunction s a
policyEvaluation mdp policy = evalVF
  where
    evalVF 0 _ = 0.0
    evalVF k s =
        sum
            (map
                 (\a ->
                       policy s a *
                       (reward mdp a s +
                        gamma mdp *
                        sum
                            (map
                                 (\s' ->
                                       transition mdp a s s' *
                                       evalVF (k - 1) s')
                                 (S.toList (states mdp)))))
                 (S.toList (actions mdp))) 
