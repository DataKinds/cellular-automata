module Automata where

import World

--applyRule :: (World a -> World a) -> World a -> World a
--applyRule r w = mapWorld r w
repeatFunction :: (a -> b) -> Int -> c
repeatFunction f n = foldl (.) (id) (replicate n f)
iterateFunction :: (a -> a) -> [(a -> a)]
iterateFunction f = iterate (f .) f

directionMapBase :: World a
directionMapBase = BaseDim (iterateFunction left) (id) (iterateFunction right)
directionMap :: World a
directionMap = HigherDim (s up) (directionMapBase) (s down)
    where
        appendDirection f' (BaseDim l c r) = BaseDim (fmap (\s -> s . f') l) (c . f') (fmap (\s -> s . f') r)
        s f = zipWith appendDirection (iterateFunction f) (repeat directionMapBase)

genUniverse :: World a -> World a
genUniverse w@(BaseDim l c r) = HigherDim () (w) (r)

mapWorld :: (Cell -> Cell) -> World a -> World a
mapWorld f (BaseDim l c r) = BaseDim (map f l) (f c) (map f r)
mapWorld f (HigherDim u c d) = HigherDim ((map . mapWorld) f u) (mapWorld f c) ((map . mapWorld) f d)
    where
        createUniverse =

rule :: World a -> Cell
rule w = On
    where
        n = getNeighborhood w
