{-# LANGUAGE ConstraintKinds #-}

module Automata where

import World

data Neighborhood = Neighborhood World

getNeighborhood :: World a -> Neighborhood
getNeighborhood (OneD ls c rs) = OneD (take radius ls) c (take radius rs)
getNeighborhood (TwoD us c ds) = TwoD (map getNeighborhood (take radius us)) (getNeighborhood c) (map getNeighborhood (take radius ds))
    where
        radius = 1

-- TODO
