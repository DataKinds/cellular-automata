module Automata where

import World

--applyRule :: (World a -> World a) -> World a -> World a
--applyRule r w = mapWorld r w

rule :: World a -> Cell
rule w = On
    where
        n = getNeighborhood w
