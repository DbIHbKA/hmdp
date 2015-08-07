-- | MDP main module.

module MDP where

import Data.Set (Set)

data MDP s a = MDP
  { states :: Set s             -- ^ is finite set of state
  , actions :: Set a            -- ^ is finite set of actions
  , transition :: a -> s -> s -> Double -- ^ is a state transition probability matrix
  , reward :: a -> s -> Double          -- ^ is a reward function
  , gamma :: Double                     -- ^ is a discount factor
  }
