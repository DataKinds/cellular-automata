module Automata where

import World

--applyRule :: (World a -> World a) -> World a -> World a
--applyRule r w = mapWorld r w
repeatFunction :: (a -> a) -> Int -> (a -> a)
repeatFunction f n = foldl (.) (id) (replicate n f)
iterateFunction :: (a -> a) -> [(a -> a)]
iterateFunction f = iterate (f .) f

directionMapBase :: World ((World a) -> (World a))
directionMapBase = BaseDim (iterateFunction left) (id) (iterateFunction right)
directionMap :: World ((World a) -> (World a))
directionMap = HigherDim (s up) (directionMapBase) (s down)
    where
        appendDirection f' (BaseDim l c r) = BaseDim (fmap (\s -> s . f') l) (c . f') (fmap (\s -> s . f') r)
        s f = zipWith appendDirection (iterateFunction f) (repeat directionMapBase)

genUniverse :: World a -> World (World a)
genUniverse w@(BaseDim l c r) = directionMapBase <*> (pure w)
genUniverse w@(HigherDim u c d) = directionMap <*> (HigherDim (repeat ws) (ws) (repeat ws))
    where
        ws = pure w

applyRule :: (World a -> a) -- The `rule` function (Neighborhood -> Output)
             -> World a     -- The world to apply the rule to
             -> World a     -- The world after update
applyRule f w = fmap (f . getNeighborhood) (genUniverse w)
