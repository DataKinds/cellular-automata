module Automata where

import World

rule :: (World a) => a -> a
rule w = w
    where
        n = getNeighborhood w
