
module TestPlanning where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Set  as S
import           MDP
import           Planning
import           PlanningM


type SGState = (Int, Int)
data SGAction
    = North
    | East
    | South
    | West
    deriving (Eq,Ord)

action :: SGState -> SGAction -> SGState
action s a
  | s == (0, 0) = s
  | s == (3, 3) = s
  | a == North = applyAction s (0, -1)
  | a == East = applyAction s (-1, 0)
  | a == South = applyAction s (0, 1)
  | a == West = applyAction s (1, 0)
  where
    applyAction c m =
        if (0 <= xx) && (xx <= 3) && (0 <= yy) && (yy <= 3)
            then (xx, yy)
            else c
      where
        xx = fst c + fst m
        yy = snd c + snd m

sgTransition :: SGAction -> SGState -> SGState -> Double
sgTransition a s s'
  | s' == (0, 0) || s' == (3, 3) = 0.0
  | s' == action s a = 1.0
  | otherwise = 0.0

sgReward :: SGAction -> SGState -> Double
sgReward _ (0,0) = 0
sgReward _ (3,3) = 0
sgReward _ _ = -1


sgMDP :: MDP SGState SGAction
sgMDP =
    MDP
    { states = S.fromList
          [(x, y) | x <- [0 .. 3]
                  , y <- [0 .. 3]]
    , actions = S.fromList [North, East, South, West]
    , transition = sgTransition
    , reward = sgReward
    , gamma = 1
    }


sgPolicy :: Policy SGState SGAction
sgPolicy _ _ = 0.25

unitTests =
    testGroup
        "Unit tests"
        [ testCase
              "results of policyEvaluation and policyEvaluationM (matrix version) must be EQ"
              (let sgmdp = sgMDP
               in map
                      (policyEvaluation sgmdp sgPolicy 3)
                      (S.toList (states sgmdp)) `compare`
                  map
                      (policyEvaluationM sgmdp sgPolicy 3)
                      (S.toList (states sgmdp)) @?=
                  EQ)]

